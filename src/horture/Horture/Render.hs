{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Horture.Render
  ( renderAssets,
    renderBackground,
    renderScene,
    indexForGif,
  )
where

import Codec.Picture.Gif
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Foreign
import Graphics.GLUtil.Camera3D as Util hiding (orientation)
import Graphics.Rendering.OpenGL as GL hiding (get, lookAt, scale)
import Horture.Asset
import Horture.Audio
import Horture.Audio.PipeWire ()
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
import Linear.Vector

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
      drawBaseQuad
      genMipMap
      currentProgram $= Nothing
      -- Flip textures for next effect. Read from written texture `w` and write to
      -- read from texture `r`.
      return (w, r)
    setLifetimeUniform (Limited s) uni = uniform uni $= s
    setLifetimeUniform Forever uni = uniform uni $= (0 :: Double)

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
      (Camera _ up _ _ camPos) = Util.fpsCamera @Float
      screenPos = s ^. pos
      lookAtM = lookAt camPos screenPos up
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
