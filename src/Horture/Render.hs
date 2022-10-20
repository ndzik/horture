{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Horture.Render
  ( renderGifs,
    renderScreen,
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
import qualified Data.Map.Strict as Map
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLUtil.Camera3D as Util
import Graphics.Rendering.OpenGL hiding (get, lookAt, scale)
import Graphics.X11
import Horture.Effect
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
              liftIO $ drawElements Triangles 6 UnsignedInt nullPtr
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

-- | renderScreen renders the captured application window. It is assumed that
-- the horture texture was already initialized at this point.
renderScreen :: (HortureLogger (Horture l)) => Double -> Object -> Horture l ()
renderScreen t s = do
  backgroundProg <- asks (^. screenProg . shader)
  screenTexUnit <- asks (^. screenProg . textureUnit)
  screenTexObject <- asks (^. screenProg . textureObject)
  (HortureState dp _ pm dim) <- get
  currentProgram $= Just backgroundProg
  activeTexture $= screenTexUnit
  textureBinding Texture2D $= Just screenTexObject
  applyScreenBehaviours t s >>= trackScreen t >>= projectScreen
  liftIO $ getWindowImage dp pm dim >>= updateWindowTexture dim
  liftIO $ drawElements Triangles 6 UnsignedInt nullPtr

applyScreenBehaviours :: (HortureLogger (Horture l)) => Double -> Object -> Horture l Object
applyScreenBehaviours t screen = do
  let bs = screen ^. behaviours
      s = foldr (\(f, bt, _) o -> f (t-bt) o) screen bs
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
  generateMipmap' Texture2D
  destroyImage i

projectionForAspectRatio :: (Float, Float) -> M44 Float
projectionForAspectRatio (ww, wh) = proj
  where
    proj = Util.projectionMatrix (Util.deg2rad 90) (ww / wh) 0.1 1000
