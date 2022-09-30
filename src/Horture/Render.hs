{-# LANGUAGE TypeApplications #-}

module Horture.Render
  ( renderGifs,
    renderScreen,
    scaleForAspectRatio,
    m44ToGLmatrix,
    identityM44,
    degToRad,
    projectionForAspectRatio,
  )
where

import Codec.Picture.Gif
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLUtil.Camera3D as Util
import Graphics.Rendering.OpenGL hiding (get)
import Graphics.X11
import Horture.Effect
import Horture.Gif
import Horture.Horture
import Horture.Object
import Horture.Scene
import Horture.State
import Horture.X11
import Linear.Matrix
import Linear.V4

renderGifs :: Double -> Map.Map GifIndex [ActiveGIF] -> Horture ()
renderGifs _ m | Map.null m = return ()
renderGifs dt m = do
  gifProg <- asks _gifProg
  modelUniform <- asks _gifModelUniform
  gifIndexUniform <- asks _gifIndexUniform
  gifTextureUnit <- asks _gifTextureUnit
  activeTexture $= gifTextureUnit
  currentProgram $= Just gifProg
  -- General preconditions are set. Render all GIFs of the same type at once.
  mapM_ (renderGifType modelUniform gifIndexUniform) . Map.toList $ m
  where
    renderGifType :: UniformLocation -> UniformLocation -> (GifIndex, [ActiveGIF]) -> Horture ()
    renderGifType _ _ (_, []) = return ()
    renderGifType modelUniform gifIndexUniform (_, gifsOfSameType@(g : _)) = do
      let HortureGIF _ _ gifTextureObject numOfImgs delays = _afGif g
          timeSinceBirth = dt - (_birth . _afObject $ g)
      textureBinding Texture2DArray $= Just gifTextureObject
      mapM_
        ( ( \o -> do
              let texOffset = indexForGif delays (timeSinceBirth * (10 ^ (2 :: Int))) numOfImgs
              liftIO $ m44ToGLmatrix (model o !*! _scale o) >>= (uniform modelUniform $=)
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
      | accumulatedTime > timeSinceBirth = i
      | otherwise = go gifDelays (accumulatedTime + fromIntegral d) (i + 1)

-- | renderScreen renders the captured application window. It is assumed that
-- the horture texture was already initialized at this point.
renderScreen :: Double -> Object -> Horture ()
renderScreen _ s = do
  backgroundProg <- asks _backgroundProg
  modelUniform <- asks _modelUniform
  screenTexUnit <- asks _screenTexUnit
  screenTexObject <- asks _screenTexObject
  (HortureState dp _ pm dim) <- get
  currentProgram $= Just backgroundProg
  activeTexture $= screenTexUnit
  textureBinding Texture2D $= Just screenTexObject
  liftIO $ getWindowImage dp pm dim >>= updateWindowTexture dim
  let scale = scaleForAspectRatio dim
      m = model s !*! scale
  liftIO $ m44ToGLmatrix m >>= (uniform modelUniform $=)
  liftIO $ drawElements Triangles 6 UnsignedInt nullPtr

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
scaleForAspectRatio (ww, wh) = model
  where
    ident = identityM44
    aspectRatio = fromIntegral ww / fromIntegral wh
    scaling =
      V4
        (V4 aspectRatio 0 0 0)
        (V4 0 1 0 0)
        (V4 0 0 1 0)
        (V4 0 0 0 1)
    model = scaling !*! ident

-- getWindowImage fetches the image of the currently captured application
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

degToRad :: Float -> Float
degToRad = (*) (pi / 180)

projectionForAspectRatio :: (Float, Float) -> M44 Float
projectionForAspectRatio (ww, wh) = proj
  where
    proj = Util.projectionMatrix (degToRad 90) (ww / wh) 1 100
