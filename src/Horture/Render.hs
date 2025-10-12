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
-- import Horture.Audio.PipeWire ()

import Control.Concurrent.STM (readTVarIO)
import Control.Lens
import Control.Monad (foldM_)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits hiding (rotate)
import Data.Default
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.RingBuffer as RingBuffer
import Data.Text (Text, unpack)
import Foreign hiding (rotate, void)
import Graphics.GLUtil.Camera3D as Util hiding (orientation)
import Graphics.Rendering.OpenGL as GL hiding (get, lookAt, rotate, scale)
import Horture.Asset
import Horture.Audio
import Horture.Character
import Horture.Effect
import Horture.Error (HortureError (HE))
import Horture.Event
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
import System.Random.Stateful (globalStdGen, randomM)

renderAssets :: forall l hdl. (HortureLogger (Horture l hdl)) => Float -> Map.Map AssetIndex [ActiveAsset] -> Horture l hdl ()
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

    drawAsset :: UniformLocation -> (Float -> Horture l hdl ()) -> ActiveAsset -> Horture l hdl ()
    drawAsset mu act a = do
      let o = _afObject a
          tB = dt - _birth o

          -- fold behaviours into a single delta
          foldOne (Behaviour _ f, bt, lt) acc =
            let τ = case lt of
                  Limited d | d > 0 -> clamp01 ((tB - bt) / d)
                  _ -> tB
             in acc <> f (0, 0, 0) τ o
          delta = foldr foldOne mempty (o ^. behaviours)

          -- apply delta to base object
          o' = applyDelta delta o

      liftIO $ m44ToGLmatrix (model o' !*! _scale o') >>= (uniform mu $=)
      act tB
      drawBaseQuad

    clamp01 :: Float -> Float
    clamp01 x = max 0 (min 1 x)

    applyDelta :: BehaviourDelta -> Object -> Object
    applyDelta (BehaviourDelta dp ds dq) obj =
      obj
        & scale %~ (!*! matScale ds)
        & orientation %~ (dq *)
        & pos %~ (^+^ dp)

    matScale :: V3 Float -> M44 Float
    matScale (V3 sx sy sz) =
      V4
        (V4 sx 0 0 0)
        (V4 0 sy 0 0)
        (V4 0 0 sz 0)
        (V4 0 0 0 1)

    noop _ = return ()

-- | indexForGif returns the index of the image for the associated GIF to be
-- viewed at the time given since birth in 100th of a second. The index is
-- clamped from [0,maxIndex].
indexForGif :: [GifDelay] -> Float -> Int -> Int
indexForGif delays timeSinceBirth maxIndex = go (cycle delays) 0 0 `mod` (maxIndex + 1)
  where
    go :: [GifDelay] -> Float -> Int -> Int
    go [] _ i = i
    go (d : gifDelays) accumulatedTime i
      | (accumulatedTime + fromIntegral d) < timeSinceBirth = go gifDelays (accumulatedTime + fromIntegral d) (i + 1)
      | otherwise = i

renderBackground :: (HortureLogger (Horture l hdl)) => Float -> Horture l hdl ()
renderBackground dt = do
  backgroundP <- asks (^. backgroundProg . shader)
  backgroundTexUnit <- asks (^. backgroundProg . textureUnit)
  timeUni <- asks (^. backgroundProg . timeUniform)
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  activeTexture $= backgroundTexUnit
  currentProgram $= Just backgroundP
  uniform @GLfloat timeUni $= (realToFrac dt)
  drawBaseQuad

-- | renderScene renders the captured application window. It is assumed that
-- the horture texture was already initialized at this point.
renderScene :: (HortureLogger (Horture l hdl), WindowPoller hdl (Horture l hdl)) => Float -> Scene -> Horture l hdl Scene
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
  -- fft <- currentFFTPeak
  let fft = (0, 0, 0) -- Disable audio-reactive effects for now.
  applySceneShaders fft t scene
  -- Final renderpass rendering scene.
  currentProgram $= Just screenP
  -- Apply behavioural effects to the scene itself.
  s <- applyScreenBehaviours fft t s >>= trackScreen t >>= projectScreen
  drawBaseQuad
  return $ scene {_screen = s}

-- | Applies all shader effects in the current scene to the captured window.
applySceneShaders :: (HortureLogger (Horture l hdl)) => FFTSnapshot -> Float -> Scene -> Horture l hdl ()
applySceneShaders fft t scene = do
  fb <- asks (^. screenProg . framebuffer)
  -- Set custom framebuffer as target for read&writes.
  bindFramebuffer Framebuffer $= fb
  -- Fronttexture here already has the captured window texture.
  readTexture <- asks (^. screenProg . textureObject)
  writeTexture <- asks (^. screenProg . backTextureObject)
  let effs = scene ^. shaders
  (finishedTex, _) <- foldrM (applyShaderEffect fft t) (readTexture, writeTexture) effs
  -- (r, w) -> (w, r) -> (r, w)
  -- Unbind post-processing framebuffer and bind default framebuffer for
  -- on-screen rendering.
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  -- Bind final texture for final rendering.
  textureBinding Texture2D $= Just finishedTex

applyShaderEffect ::
  (HortureLogger (Horture l hdl)) =>
  FFTSnapshot ->
  Float ->
  (ShaderEffect, Float, Lifetime) ->
  (TextureObject, TextureObject) ->
  Horture l hdl (TextureObject, TextureObject)
applyShaderEffect (bass, mids, highs) t (eff, birth, lt) buffers = do
  (tw, th) <- liftIO . (forBoth fromIntegral <$>) . readTVarIO =<< gets (^. dim)
  shaderProgs <-
    asks (Map.lookup eff . (^. screenProg . shaderEffects)) >>= \case
      Nothing -> throwError $ HE "unhandled shadereffect encountered"
      Just sp -> return sp
  foldrM (go (tw, th)) buffers shaderProgs
  where
    go (w, h) prog (readTexture, writeTexture) = do
      -- Read from texture r.
      textureBinding Texture2D $= Just readTexture
      -- Write to texture w.
      liftIO $ do
        framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D writeTexture 0
        GL.viewport $= (Position 0 0, Size w h)
      currentProgram $= Just (prog ^. shader)
      setLifetimeUniform lt (prog ^. lifetimeUniform)
      liftIO . withArray [bass, mids, highs] $ uniformv (prog ^. frequenciesUniform) 3
      uniform @Float (prog ^. dtUniform) $= (realToFrac $ t - birth)
      newRandomNumber >>= (uniform (prog ^. randomUniform) $=)
      drawBaseQuad
      currentProgram $= Nothing
      -- Flip textures for next effect. Read from written texture `w` and write to
      -- read from texture `r`.
      return (writeTexture, readTexture)
    setLifetimeUniform (Limited s) uni = uniform uni $= s
    setLifetimeUniform Forever uni = uniform uni $= (0 :: Float)

newRandomNumber :: Horture l hdl Float
newRandomNumber = liftIO $ randomM globalStdGen

renderEventList :: (HortureLogger (Horture l hdl)) => Float -> Horture l hdl ()
renderEventList timeNow = do
  gets (^. eventList) >>= liftIO . RingBuffer.toList >>= \evs -> do
    let numOfLines = length evs - 1
    go numOfLines 0 $ map (\pe@(PastEvent bt _ _) -> (bt, show pe)) evs
  where
    height = round $ fromIntegral (characterHeight + lineSpacing) * baseScale
    showTime = 16
    lineSpacing = round $ 10 * baseScale
    go :: (HortureLogger (Horture l hdl)) => Int -> Int -> [(Float, String)] -> Horture l hdl ()
    go _ _ [] = return ()
    go numOfLines i ((bt, l) : ls) = do
      renderText l (lineSpacing, lineSpacing + numOfLines * height - height * i) . realToFrac $ 1 - ((timeNow - bt) / showTime)
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
  (_, top) <- liftIO . (forBoth fromIntegral <$>) . readTVarIO =<< gets (^. dim)
  let height = round $ fromIntegral (characterHeight + lineSpacing) * baseScale
  go top height effs 0
  where
    go _ _ [] _ = return ()
    go top height ((eff, c) : rs) l = do
      renderText (show c ++ "x " ++ unpack eff) (x, top - height - height * l) 1.0
      go top height rs (l + 1)
    x = 10
    lineSpacing = round $ 10 * baseScale

renderText :: (HortureLogger (Horture l hdl)) => String -> (Int, Int) -> Float -> Horture l hdl ()
renderText txt posi opacity = do
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  fp <- asks (^. fontProg)
  let mu = fp ^. modelUniform
      tu = fp ^. textureUnit
      ou = fp ^. opacityUniform
      chs = fp ^. chars
  let word = WordObject def $ mapMaybe (`Map.lookup` chs) txt
  activeTexture $= tu
  currentProgram $= Just (fp ^. shader)
  uniform ou $= opacity
  renderWord mu posi word
  where
    renderWord :: (HortureLogger (Horture l hdl)) => UniformLocation -> (Int, Int) -> WordObject -> Horture l hdl ()
    renderWord mu (x, y) word = do
      let renderCharacter :: (HortureLogger (Horture l hdl)) => Int -> Character -> Horture l hdl Int
          renderCharacter adv ch = do
            textureBinding Texture2D $= Just (ch ^. textureID)
            (screenW, screenH) <- liftIO . (forBoth fromIntegral <$>) . readTVarIO =<< gets (^. dim)
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

applyScreenBehaviours ::
  (HortureLogger (Horture l hdl)) =>
  FFTSnapshot -> Float -> Object -> Horture l hdl Object
applyScreenBehaviours fft t screen = do
  let foldOne (Behaviour _ f, bt, lt) acc =
        let τ = case lt of Limited d | d > 0 -> clamp01 ((t - bt) / d); _ -> t
         in acc <> f fft τ screen
      delta = foldr foldOne mempty (screen ^. behaviours)
  resetScreen (applyDelta delta screen)
  where
    clamp01 = max 0 . min 1
    applyDelta (BehaviourDelta dp ds dq) o =
      o
        & scale %~ (!*! matScale ds)
        & orientation %~ (dq *)
        & pos %~ (^+^ dp)

    matScale (V3 sx sy sz) =
      V4
        (V4 sx 0 0 0)
        (V4 0 sy 0 0)
        (V4 0 0 sz 0)
        (V4 0 0 0 1)

resetScreen :: Object -> Horture l hdl Object
resetScreen s = do
  dim <- liftIO . (forBoth fromIntegral <$>) . readTVarIO =<< gets (^. dim)
  let s' = s & scale %~ \cs -> lerp 0.1 (scaleForAspectRatio dim) cs
      s'' = s' & orientation %~ \og -> slerp og (Quaternion 1.0 (V3 0 0 0)) 0.01
      s''' = s'' & pos %~ \op -> lerp 0.1 (V3 0 0 (-1)) op
  return s'''

trackScreen :: (HortureLogger (Horture l hdl)) => Float -> Object -> Horture l hdl Object
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
  (w, h) <- liftIO . (forBoth fromIntegral <$>) . readTVarIO =<< gets (^. dim)
  let proj = projectionForAspectRatio (w, h)
  liftIO $ m44ToGLmatrix proj >>= (uniform projectionUniform $=)
  liftIO $ m44ToGLmatrix (model s) >>= (uniform modelUniform $=)
  return s

forBoth :: (a -> b) -> (a, a) -> (b, b)
forBoth f (x, y) = (f x, f y)
