{-# LANGUAGE TypeApplications #-}

module Horture.Render
  ( renderObjects,
    renderScreen,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL hiding (get)
import Graphics.X11
import Horture.Horture
import Horture.Object
import Horture.State
import Horture.X11
import Linear.Matrix
import Linear.V4

renderObjects :: Double -> [Object] -> Horture ()
renderObjects _ [] = return ()
renderObjects dt os = do
  gifProg <- asks _gifProg
  gifTex <- asks _gifTexUnit
  gifTexObject <- asks _gifTexObject
  gifIndex <- asks _gifIndex
  modelUniform <- asks _gifModelUniform
  liftIO $ currentProgram $= Just gifProg
  activeTexture $= gifTex
  textureBinding Texture2DArray $= Just gifTexObject
  go gifIndex modelUniform os
  where
    go :: UniformLocation -> UniformLocation -> [Object] -> Horture ()
    go _ _ [] = return ()
    go gifIndex modelUniform (o : os) = do
      let elapsedms = round $ dt * (10 ^ (2 :: Int))
          i = (elapsedms `div` (10 * _delay o)) `mod` _textureLength o
      liftIO $
        m44ToGLmatrix
          ( model o
              !*! V4
                (V4 0.5 0 0 0)
                (V4 0 0.5 0 0)
                (V4 0 0 0.5 0)
                (V4 0 0 0 1)
          )
          >>= (uniform modelUniform $=)
      liftIO $ uniform gifIndex $= fromIntegral @_ @GLint i
      liftIO $ drawElements Triangles 6 UnsignedInt nullPtr
      go gifIndex modelUniform os

-- renderScreen renders the captured application window. It is assumed that the
-- horture texture was already initialized at this point.
renderScreen :: Double -> Object -> Horture ()
renderScreen _ s = do
  backgroundProg <- asks _backgroundProg
  modelUniform <- asks _modelUniform
  screenTexUnit <- asks _screenTexUnit
  screenTexObject <- asks _screenTexObject
  (HortureState dp _ pm dim) <- get
  liftIO $ currentProgram $= Just backgroundProg
  activeTexture $= screenTexUnit
  textureBinding Texture2D $= Just screenTexObject
  liftIO $ getWindowImage dp pm dim >>= updateWindowTexture dim
  let scale = scaleForAspectRatio dim
      m = model s !*! scale
  liftIO $ m44ToGLmatrix m >>= (uniform modelUniform $=)
  liftIO $ drawElements Triangles 6 UnsignedInt nullPtr

-- m44ToGLmatrix converts the row based representation of M44 to a GLmatrix
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
