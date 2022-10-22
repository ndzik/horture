{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Horture.Render
  ( renderGifs,
    renderScene,
    scaleForAspectRatio,
    m44ToGLmatrix,
    identityM44,
    indexForGif,
    projectionForAspectRatio,
  )
where

import Codec.Picture.Gif
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLUtil.Camera3D as Util
import Graphics.Rendering.OpenGL as GL hiding (get, lookAt, scale)
import Graphics.X11
import Horture.Effect
import Horture.Error (HortureError (HE))
import Horture.Gif
import Horture.Horture
import Horture.Logging
import Horture.Object
import Horture.Program
import Horture.Scene
import Horture.State
import Horture.X11
import Linear.Matrix
import Linear.Projection
import Linear.V4

renderGifs :: (HortureLogger (Horture l)) => Double -> Map.Map GifIndex [ActiveGif] -> Horture l ()
renderGifs _ m | Map.null m = return ()
renderGifs dt m = do
  prog <- asks (^. gifProg . shader)
  modelUniform <- asks (^. gifProg . modelUniform)
  gifIndexUniform <- asks (^. gifProg . indexUniform)
  gifTextureUnit <- asks (^. gifProg . textureUnit)
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  activeTexture $= gifTextureUnit
  currentProgram $= Just prog
  -- General preconditions are set. Render all GIFs of the same type at once.
  mapM_ (renderGifType modelUniform gifIndexUniform) . Map.toList $ m
  where
    renderGifType :: (HortureLogger (Horture l)) => UniformLocation -> UniformLocation -> (GifIndex, [ActiveGif]) -> Horture l ()
    renderGifType _ _ (_, []) = return ()
    renderGifType modelUniform gifIndexUniform (_, gifsOfSameType@(g : _)) = do
      let HortureGif _ _ gifTextureObject numOfImgs delays = _afGif g
      textureBinding Texture2DArray $= Just gifTextureObject
      mapM_
        ( ( \o -> do
              let bs = o ^. behaviours
                  timeSinceBirth = dt - _birth o
                  o' = foldr (\(f, _, _) o -> f timeSinceBirth o) o bs
                  texOffset = indexForGif delays (timeSinceBirth * (10 ^ (2 :: Int))) numOfImgs
              liftIO $ m44ToGLmatrix (model o' !*! _scale o') >>= (uniform modelUniform $=)
              uniform gifIndexUniform $= fromIntegral @Int @GLint (fromIntegral texOffset)
              drawBaseQuad
          )
            . _afObject
        )
        gifsOfSameType

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

-- | renderScene renders the captured application window. It is assumed that
-- the horture texture was already initialized at this point.
renderScene :: (HortureLogger (Horture l)) => Double -> Scene -> Horture l ()
renderScene t scene = do
  let s = scene ^. screen
  backgroundProg <- asks (^. screenProg . shader)
  (HortureState dp _ pm dim) <- get
  screenTexUnit <- asks (^. screenProg . textureUnit)
  sourceTexObject <- asks (^. screenProg . textureObject)
  -- Bind texture which we read from.
  activeTexture $= screenTexUnit
  textureBinding Texture2D $= Just sourceTexObject
  captureApplicationFrame dp pm dim
  -- Apply shaders to captured texture.
  applySceneShaders t scene
  -- Final renderpass rendering scene.
  currentProgram $= Just backgroundProg
  -- Apply behavioural effects to the scene itself.
  applyScreenBehaviours t s >>= trackScreen t >>= projectScreen
  drawBaseQuad

-- | Updates currently bound texture with the pixeldata of the frame for the
-- captured application.
captureApplicationFrame ::
  (HortureLogger (Horture l)) =>
  Display ->
  Drawable ->
  (Int, Int) ->
  Horture l ()
captureApplicationFrame dp pm dim =
  liftIO $ getWindowImage dp pm dim >>= updateWindowTexture dim

-- | Applies all shader effects in the current scene to the captured window.
applySceneShaders :: (HortureLogger (Horture l)) => Double -> Scene -> Horture l ()
applySceneShaders t scene = do
  fb <- asks (^. screenProg . framebuffer)
  -- Set custom framebuffer as target for read&writes.
  bindFramebuffer Framebuffer $= fb
  frontTexture <- asks (^. screenProg . textureObject)
  backTexture <- asks (^. screenProg . backTextureObject)
  -- Use tmp texture with correct dimensions to enable ping-ponging.
  textureBinding Texture2D $= Just backTexture
  let effs = scene ^. shaders
  (finishedTex, _) <- foldrM (applyShaderEffect t) (frontTexture, backTexture) effs
  -- Unbind post-processing framebuffer and bind default framebuffer for
  -- on-screen rendering.
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  -- Bind final texture for final rendering.
  textureBinding Texture2D $= Just finishedTex
  liftIO $ generateMipmap' Texture2D

applyShaderEffect ::
  (HortureLogger (Horture l)) =>
  Double ->
  (ShaderEffect, Double, Lifetime) ->
  (TextureObject, TextureObject) ->
  Horture l (TextureObject, TextureObject)
applyShaderEffect _t (eff, _birth, _lt) buffers = do
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
      currentProgram $= Just prog
      drawBaseQuad
      genMipMap
      currentProgram $= Nothing
      -- Flip textures for next effect. Read from written texture `w` and write to
      -- read from texture `r`.
      return (w, r)

applyScreenBehaviours :: (HortureLogger (Horture l)) => Double -> Object -> Horture l Object
applyScreenBehaviours t screen = do
  let bs = screen ^. behaviours
      s = foldr (\(f, bt, _) o -> f (t - bt) o) screen bs
  return s

trackScreen :: (HortureLogger (Horture l)) => Double -> Object -> Horture l Object
trackScreen _ screen = do
  viewUniform <- asks (^. screenProg . viewUniform)
  let s = screen
      (Camera _ up _ _ camPos) = Util.fpsCamera @Float
      screenPos = s ^. pos
      lookAtM = lookAt camPos screenPos up
  liftIO $ m44ToGLmatrix lookAtM >>= (uniform viewUniform $=)
  return s

projectScreen :: (HortureLogger (Horture l)) => Object -> Horture l ()
projectScreen s = do
  modelUniform <- asks (^. screenProg . modelUniform)
  projectionUniform <- asks (^. screenProg . projectionUniform)
  dim@(w, h) <- gets (^. dim)
  let s' = s & scale .~ scaleForAspectRatio dim
      proj = projectionForAspectRatio (fromIntegral w, fromIntegral h)
  liftIO $ m44ToGLmatrix proj >>= (uniform projectionUniform $=)
  liftIO $ m44ToGLmatrix (model s') >>= (uniform modelUniform $=)

drawBaseQuad :: Horture l ()
drawBaseQuad = liftIO $ drawElements Triangles 6 UnsignedInt nullPtr

genMipMap :: Horture l ()
genMipMap = liftIO $ generateMipmap' Texture2D

-- | m44ToGLmatrix converts the row based representation of M44 to a GLmatrix
-- representation which is column based.
m44ToGLmatrix :: (Show a, MatrixComponent a) => M44 a -> IO (GLmatrix a)
m44ToGLmatrix m = withNewMatrix ColumnMajor (\p -> poke (castPtr p) m')
  where
    m' = transpose m

identityM44 :: M44 Float
identityM44 =
  V4
    (V4 1 0 0 0)
    (V4 0 1 0 0)
    (V4 0 0 1 0)
    (V4 0 0 0 1)

scaleForAspectRatio :: (Int, Int) -> M44 Float
scaleForAspectRatio (ww, wh) = scaling
  where
    aspectRatio = fromIntegral ww / fromIntegral wh
    scaling =
      V4
        (V4 aspectRatio 0 0 0)
        (V4 0 1 0 0)
        (V4 0 0 1 0)
        (V4 0 0 0 1)

-- | getWindowImage fetches the image of the currently captured application
-- window.
getWindowImage :: Display -> Drawable -> (Int, Int) -> IO Image
getWindowImage dp pm (w, h) =
  getImage
    dp
    pm
    1
    1
    (fromIntegral w)
    (fromIntegral h)
    0xFFFFFFFF
    zPixmap

-- | updateWindowTexture updates the OpenGL texture for the captured window
-- using the given dimensions together with the source image as a data source.
updateWindowTexture :: (Int, Int) -> Image -> IO ()
updateWindowTexture (w, h) i = do
  src <- ximageData i
  let pd = PixelData BGRA UnsignedInt8888Rev src
  texSubImage2D
    Texture2D
    0
    (TexturePosition2D 0 0)
    (TextureSize2D (fromIntegral w) (fromIntegral h))
    pd
  destroyImage i

projectionForAspectRatio :: (Float, Float) -> M44 Float
projectionForAspectRatio (ww, wh) = proj
  where
    proj = Util.projectionMatrix (Util.deg2rad 90) (ww / wh) 0.1 1000
