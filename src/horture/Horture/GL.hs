module Horture.GL
  ( m44ToGLmatrix,
    identityM44,
    scaleForAspectRatio,
    drawBaseQuad,
    genMipMap,
    projectionForAspectRatio,
  )
where

import Control.Monad.IO.Class
import Graphics.GLUtil.Camera3D as Util
import Graphics.Rendering.OpenGL as GL hiding (get, lookAt, scale)
import Horture.Horture
import Linear.Matrix
import Linear.V4
import Foreign.Storable
import Foreign.Ptr

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

drawBaseQuad :: Horture l hdl ()
drawBaseQuad = liftIO $ drawElements Triangles 6 UnsignedInt nullPtr

genMipMap :: Horture l hdl ()
genMipMap = liftIO $ generateMipmap' Texture2D

projectionForAspectRatio :: (Float, Float) -> M44 Float
projectionForAspectRatio (ww, wh) = proj
  where
    proj = Util.projectionMatrix (Util.deg2rad 90) (ww / wh) 0.1 1000
