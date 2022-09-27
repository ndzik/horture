module Horture.State
  ( HortureStatic (..),
    HortureState (..),
  )
where

import Graphics.Rendering.OpenGL hiding (get)
import Graphics.X11

data HortureStatic = HortureStatic
  { _backgroundProg :: Program,
    _gifProg :: Program,
    _modelUniform :: UniformLocation,
    _viewUniform :: UniformLocation,
    _projUniform :: UniformLocation,
    _planeVertexLocation :: AttribLocation,
    _planeTexLocation :: AttribLocation,
    _screenTexUnit :: TextureUnit,
    _gifTexUnit :: TextureUnit,
    _gifIndex :: UniformLocation,
    _backgroundColor :: Color4 Float
  }
  deriving (Show)

data HortureState = HortureState
  { _display :: Display,
    _capture :: Drawable,
    _dim :: (Int, Int)
  }
