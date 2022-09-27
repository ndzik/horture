{-# LANGUAGE LambdaCase #-}
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
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Horture.Horture
import Horture.Object
import Horture.Scene
import Horture.State
import Horture.X11
import Linear.Matrix


renderObjects :: Double -> [Object] -> Horture ()
renderObjects dt os = do
  gifProg <- asks _gifProg
  gifTex <- asks _gifTexUnit
  gifIndex <- asks _gifIndex
  modelUniform <- asks _modelUniform
  liftIO $ currentProgram $= Just gifProg
  activeTexture $= gifTex
  go gifIndex modelUniform os
  liftIO $ drawElements Triangles 6 UnsignedInt nullPtr
  where
    go :: UniformLocation -> UniformLocation -> [Object] -> Horture ()
    go _ _ [] = return ()
    go gifIndex modelUniform (o : os) = do
      let elapsedms = round $ dt * (10 ^ (2 :: Int))
          i = (elapsedms `div` (10 * _delay o)) `mod` _textureLength o
      liftIO $ m44ToGLmatrix (model o) >>= (uniform modelUniform $=)
      liftIO $ uniform gifIndex $= fromIntegral @_ @GLint i
      go gifIndex modelUniform os

-- renderScreen renders the captured application window. It is assumed that the
-- horture texture was already initialized at this point.
renderScreen :: Double -> Object -> Horture ()
renderScreen _ s = do
  modelUniform <- asks _modelUniform
  backgroundProg <- asks _backgroundProg
  screenTexUnit <- asks _screenTexUnit
  (HortureState dp pm dim) <- get
  liftIO $ currentProgram $= Just backgroundProg
  activeTexture $= screenTexUnit
  liftIO $ getWindowImage dp pm dim >>= updateWindowTexture dim
  liftIO $ m44ToGLmatrix (model s) >>= (uniform modelUniform $=)
  liftIO $ drawElements Triangles 6 UnsignedInt nullPtr

-- m44ToGLmatrix converts the row based representation of M44 to a GLmatrix
-- representation which is column based.
m44ToGLmatrix :: (Show a, MatrixComponent a) => M44 a -> IO (GLmatrix a)
m44ToGLmatrix m = withNewMatrix ColumnMajor (\p -> poke (castPtr p) m')
  where
    m' = transpose m

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
  destroyImage i
