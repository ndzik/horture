{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Horture.Render
  ( renderAssets,
    renderBackground,
    renderText,
    renderActiveEffectText,
    renderEventList,
    renderScene,
    indexForGif,
  )
where

import Codec.Picture.Gif
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits hiding (rotate)
import Data.Default
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text, unpack)
import Foreign hiding (rotate, void)
import Graphics.GLUtil.Camera3D as Util hiding (orientation)
import Graphics.Rendering.OpenGL as GL hiding (get, lookAt, rotate, scale)
import Horture.Asset
import Horture.Audio
import Horture.Audio.PipeWire ()
import Horture.Character
import Horture.Effect
import Horture.Error (HortureError (HE))
import Horture.GL
import Horture.Horture
import Horture.Logging
import Horture.Object
import Horture.Program
import Horture.Scene
import Horture.State
import Horture.WindowGrabber
import Linear.Matrix
import Linear.Projection
import Linear.Quaternion
import Linear.V3
import Linear.V4
import Linear.Vector
import qualified RingBuffers.Lifted as RingBuffer
import System.Random.Stateful (globalStdGen, randomM)

renderAssets :: (HortureLogger (Horture l hdl)) => Double -> Map.Map AssetIndex [ActiveAsset] -> Horture l hdl ()
renderAssets _ m | Map.null m = return ()
renderAssets dt m = do
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  -- General preconditions are set. Render all Assets.
  mapM_ renderAssetType . Map.toList $ m
  where
    renderAssetType :: (HortureLogger (Horture l hdl)) => (AssetIndex, [ActiveAsset]) -> Horture l hdl ()
    renderAssetType (_, []) = return ()
    renderAssetType (_, assetsOfSameType@(g : _)) = do
      case _afAsset g of
        HortureGif _ _ gifTextureObject numOfImgs delays -> do
          prog <- asks (^. dynamicImageProg . gifProgram . shader)
          modelUniform <- asks (^. dynamicImageProg . gifProgram . modelUniform)
          gifIndexUniform <- asks (^. dynamicImageProg . gifProgram . indexUniform)
          gifTextureUnit <- asks (^. dynamicImageProg . gifProgram . textureUnit)
          activeTexture $= gifTextureUnit
          currentProgram $= Just prog
          textureBinding Texture2DArray $= Just gifTextureObject
          mapM_
            ( drawAsset modelUniform $ \timeSinceBirth ->
                let texOffset = indexForGif delays (timeSinceBirth * (10 ^ (2 :: Int))) numOfImgs
                 in uniform gifIndexUniform $= fromIntegral @Int @GLint (fromIntegral texOffset)
            )
            assetsOfSameType
        HortureImage _ _ imageTextureObject -> do
          imgProg <- asks (^. dynamicImageProg . imageProgram . shader)
          modelUniform <- asks (^. dynamicImageProg . imageProgram . modelUniform)
          imgTextureUnit <- asks (^. dynamicImageProg . imageProgram . textureUnit)
          activeTexture $= imgTextureUnit
          currentProgram $= Just imgProg
          textureBinding Texture2D $= Just imageTextureObject
          mapM_ (drawAsset modelUniform noop) assetsOfSameType

    drawAsset mu act a = do
      let o = _afObject a
          bs = o ^. behaviours
          timeSinceBirth = dt - _birth o
          o' = foldr (\(Behaviour _ f, _, _) o -> f (0, 0, 0) timeSinceBirth o) o bs
      liftIO $ m44ToGLmatrix (model o' !*! _scale o') >>= (uniform mu $=)
      _ <- act timeSinceBirth
      drawBaseQuad

    noop _ = return ()

-- | indexForGif returns the index of the image for the associated GIF to be
-- viewed at the time given since birth in 100th of a second. The index is
-- clamped from [0,maxIndex].
indexForGif :: [GifDelay] -> Double -> Int -> Int
indexForGif delays timeSinceBirth maxIndex = go (cycle delays) 0 0 `mod` (maxIndex + 1)
  where
    go :: [GifDelay] -> Double -> Int -> Int
    go [] _ i = i
    go (d : gifDelays) accumulatedTime i
      | (accumulatedTime + fromIntegral d) < timeSinceBirth = go gifDelays (accumulatedTime + fromIntegral d) (i + 1)
      | otherwise = i

renderBackground :: (HortureLogger (Horture l hdl)) => Double -> Horture l hdl ()
renderBackground dt = do
  backgroundP <- asks (^. backgroundProg . shader)
  backgroundTexUnit <- asks (^. backgroundProg . textureUnit)
  timeUni <- asks (^. backgroundProg . timeUniform)
  activeTexture $= backgroundTexUnit
  uniform timeUni $= dt
  currentProgram $= Just backgroundP
  drawBaseQuad

-- | renderScene renders the captured application window. It is assumed that
-- the horture texture was already initialized at this point.
renderScene :: (HortureLogger (Horture l hdl), WindowPoller hdl (Horture l hdl)) => Double -> Scene -> Horture l hdl Scene
renderScene t scene = do
  let s = scene ^. screen
  screenP <- asks (^. screenProg . shader)
  screenTexUnit <- asks (^. screenProg . textureUnit)
  sourceTexObject <- asks (^. screenProg . textureObject)
  -- Bind texture which we will read from.
  activeTexture $= screenTexUnit
  textureBinding Texture2D $= Just sourceTexObject
  -- Fetch the next frame.
  nextFrame
  -- Apply shaders to captured texture.
  fft <- currentFFTPeak
  applySceneShaders fft t scene
  -- Final renderpass rendering scene.
  currentProgram $= Just screenP
  -- Apply behavioural effects to the scene itself.
  s <- applyScreenBehaviours fft t s >>= trackScreen t >>= projectScreen
  drawBaseQuad
  return $ scene {_screen = s}

-- | Applies all shader effects in the current scene to the captured window.
applySceneShaders :: (HortureLogger (Horture l hdl)) => FFTSnapshot -> Double -> Scene -> Horture l hdl ()
applySceneShaders fft t scene = do
  fb <- asks (^. screenProg . framebuffer)
  -- Set custom framebuffer as target for read&writes.
  bindFramebuffer Framebuffer $= fb
  frontTexture <- asks (^. screenProg . textureObject)
  backTexture <- asks (^. screenProg . backTextureObject)
  -- Use tmp texture with correct dimensions to enable ping-ponging.
  textureBinding Texture2D $= Just backTexture
  let effs = scene ^. shaders
  (finishedTex, _) <- foldrM (applyShaderEffect fft t) (frontTexture, backTexture) effs
  -- Unbind post-processing framebuffer and bind default framebuffer for
  -- on-screen rendering.
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  -- Bind final texture for final rendering.
  textureBinding Texture2D $= Just finishedTex
  liftIO $ generateMipmap' Texture2D

applyShaderEffect ::
  (HortureLogger (Horture l hdl)) =>
  FFTSnapshot ->
  Double ->
  (ShaderEffect, Double, Lifetime) ->
  (TextureObject, TextureObject) ->
  Horture l hdl (TextureObject, TextureObject)
applyShaderEffect (bass, mids, highs) t (eff, birth, lt) buffers = do
  shaderProgs <-
    asks (Map.lookup eff . (^. screenProg . shaderEffects)) >>= \case
      Nothing -> throwError $ HE "unhandled shadereffect encountered"
      Just sp -> return sp
  foldrM go buffers shaderProgs
  where
    go prog (r, w) = do
      -- Read from texture r.
      textureBinding Texture2D $= Just r
      genMipMap -- Mipmap generation has to happen for everyframe.
      -- Write to texture w.
      liftIO $ framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D w 0
      currentProgram $= Just (prog ^. shader)
      setLifetimeUniform lt (prog ^. lifetimeUniform)
      liftIO . withArray [bass, mids, highs] $ uniformv (prog ^. frequenciesUniform) 3
      uniform (prog ^. dtUniform) $= t - birth
      newRandomNumber >>= (uniform (prog ^. randomUniform) $=)
      drawBaseQuad
      genMipMap
      currentProgram $= Nothing
      -- Flip textures for next effect. Read from written texture `w` and write to
      -- read from texture `r`.
      return (w, r)
    setLifetimeUniform (Limited s) uni = uniform uni $= s
    setLifetimeUniform Forever uni = uniform uni $= (0 :: Double)

newRandomNumber :: Horture l hdl Double
newRandomNumber = liftIO $ randomM globalStdGen

renderEventList :: (HortureLogger (Horture l hdl)) => Horture l hdl ()
renderEventList = do
  gets (^. eventList) >>= liftIO . RingBuffer.toList >>= \evs -> do
    let numOfLines = length evs
    go numOfLines 0 $ map show evs
  where
    height = round $ fromIntegral (characterHeight + lineSpacing) * baseScale
    lineSpacing = round $ 10 * baseScale
    go :: (HortureLogger (Horture l hdl)) => Int -> Int -> [String] -> Horture l hdl ()
    go _ _ [] = return ()
    go numOfLines i (l : ls) = do
      renderText l (lineSpacing, numOfLines * height - height * i)
      go numOfLines (i + 1) ls

renderActiveEffectText :: (HortureLogger (Horture l hdl)) => Scene -> Horture l hdl ()
renderActiveEffectText s = do
  let bs = foldr (\(bh, _, _) -> incrementOrInsert (toTitle bh) []) [] $ s ^. screen . behaviours
      ss = foldr (\(sh, _, _) -> incrementOrInsert (toTitle sh) []) [] $ s ^. shaders
      effs = case ("RandomGifOrImage", sum $ Map.map length (_assets s)) of
        (_, 0) -> bs ++ ss
        as -> as : bs ++ ss
  renderEffectList effs
  where
    incrementOrInsert :: Text -> [(Text, Int)] -> [(Text, Int)] -> [(Text, Int)]
    incrementOrInsert sh ds [] = (sh, 1) : ds
    incrementOrInsert sh ds ((fs, c) : r)
      | sh == fs = reverse r ++ (sh, c + 1) : ds
      | otherwise = incrementOrInsert sh ((fs, c) : ds) r

renderEffectList :: (HortureLogger (Horture l hdl)) => [(Text, Int)] -> Horture l hdl ()
renderEffectList effs = do
  (_, top) <- gets (^. dim)
  let height = round $ fromIntegral (characterHeight + lineSpacing) * baseScale
  go top height effs 0
  where
    go _ _ [] _ = return ()
    go top height ((eff, c) : rs) l = do
      renderText (show c ++ "x " ++ unpack eff) (x, top - height - height * l)
      go top height rs (l + 1)
    x = 10
    lineSpacing = round $ 10 * baseScale

renderText :: (HortureLogger (Horture l hdl)) => String -> (Int, Int) -> Horture l hdl ()
renderText txt posi = do
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  fp <- asks (^. fontProg)
  let mu = fp ^. modelUniform
      tu = fp ^. textureUnit
      chs = fp ^. chars
  let word = WordObject def $ mapMaybe (`Map.lookup` chs) txt
  activeTexture $= tu
  currentProgram $= Just (fp ^. shader)
  renderWord mu posi word
  where
    renderWord :: (HortureLogger (Horture l hdl)) => UniformLocation -> (Int, Int) -> WordObject -> Horture l hdl ()
    renderWord mu (x, y) word = do
      let renderCharacter :: (HortureLogger (Horture l hdl)) => Int -> Character -> Horture l hdl Int
          renderCharacter adv ch = do
            textureBinding Texture2D $= Just (ch ^. textureID)
            (screenW, screenH) <- gets (^. dim)
            let bearingX = fromIntegral (ch ^. bearing . _x) * baseScale
                bearingY = round $ fromIntegral (ch ^. bearing . _y) * baseScale
                w = round $ fromIntegral (ch ^. size . _x) * baseScale
                h = round $ fromIntegral (ch ^. size . _y) * baseScale
                xOffset = round bearingX
                yOffset = fromIntegral (h - bearingY)
                x' = adv + xOffset + (w `div` 2)
                y' = y - yOffset + (h `div` 2)
                (realX, realY) = toScreenCoordinates (x', y') (screenW, screenH)
                trans = mkTransformation @Float (word ^. object . orientation) (V3 realX realY 0)
                xScale = fromIntegral w / fromIntegral screenW
                yScale = fromIntegral h / fromIntegral screenH
                scale =
                  V4
                    (V4 xScale 0 0 0)
                    (V4 0 yScale 0 0)
                    (V4 0 0 1 0)
                    (V4 0 0 0 1)
            liftIO $ m44ToGLmatrix (trans !*! scale) >>= (uniform mu $=)
            drawBaseQuad
            return $ adv + round (fromIntegral (shiftR (ch ^. advance) 6) * baseScale)
      foldM_ renderCharacter x $ word ^. letters
    toScreenCoordinates :: (Int, Int) -> (Int, Int) -> (Float, Float)
    toScreenCoordinates (x, y) (w, h) =
      let halfW = fromIntegral w / 2
          halfH = fromIntegral h / 2
          wPerPixel = 2 / fromIntegral w
          hPerPixel = 2 / fromIntegral h
          pixelX = fromIntegral x
          pixelY = fromIntegral y
       in ((pixelX - halfW) * wPerPixel, (pixelY - halfH) * hPerPixel)

applyScreenBehaviours :: (HortureLogger (Horture l hdl)) => FFTSnapshot -> Double -> Object -> Horture l hdl Object
applyScreenBehaviours fft t screen = do
  let bs = screen ^. behaviours
      s =
        foldr
          ( \(Behaviour _ f, bt, lt) o -> case lt of
              Limited lt -> f fft ((t - bt) / lt) o
              Forever -> f fft t o
          )
          screen
          bs
  resetScreen s

resetScreen :: Object -> Horture l hdl Object
resetScreen s = do
  dim <- gets (^. dim)
  let s' = s & scale %~ \cs -> lerp 0.1 (scaleForAspectRatio dim) cs
      s'' = s' & orientation %~ \og -> slerp og (Quaternion 1.0 (V3 0 0 0)) 0.01
      s''' = s'' & pos %~ \op -> lerp 0.1 (V3 0 0 (-1)) op
  return s'''

trackScreen :: (HortureLogger (Horture l hdl)) => Double -> Object -> Horture l hdl Object
trackScreen _ screen = do
  viewUniform <- asks (^. screenProg . viewUniform)
  let s = screen
      (Camera fwd up _ curOrientation camPos) = Util.fpsCamera @Float
      screenPos = s ^. pos
      curScreenPos = rotate curOrientation fwd
      lookAtM = lookAt camPos (lerp 0.3 screenPos curScreenPos) up
  liftIO $ m44ToGLmatrix lookAtM >>= (uniform viewUniform $=)
  return s

projectScreen :: (HortureLogger (Horture l hdl)) => Object -> Horture l hdl Object
projectScreen s = do
  modelUniform <- asks (^. screenProg . modelUniform)
  projectionUniform <- asks (^. screenProg . projectionUniform)
  (w, h) <- gets (^. dim)
  let proj = projectionForAspectRatio (fromIntegral w, fromIntegral h)
  liftIO $ m44ToGLmatrix proj >>= (uniform projectionUniform $=)
  liftIO $ m44ToGLmatrix (model s) >>= (uniform modelUniform $=)
  return s
