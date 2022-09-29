module Horture.State
  ( HortureStatic (..),
    HortureState (..),
  )
where

import qualified Data.Map.Strict as Map
import Graphics.Rendering.OpenGL hiding (get)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Horture.Gif

data HortureStatic = HortureStatic
  { _backgroundProg :: !Program,
    _gifProg :: !Program,
    _modelUniform :: !UniformLocation,
    _viewUniform :: !UniformLocation,
    _projUniform :: !UniformLocation,
    _planeVertexLocation :: !AttribLocation,
    _planeTexLocation :: !AttribLocation,
    _screenTexUnit :: !TextureUnit,
    _screenTexObject :: !TextureObject,
    _gifIndexUniform :: !UniformLocation,
    _gifModelUniform :: !UniformLocation,
    _loadedGifs :: !(Map.Map FilePath HortureGIF),
    _glWin :: !GLFW.Window,
    _backgroundColor :: !(Color4 Float)
  }
  deriving (Show)

data HortureState = HortureState
  { _display :: !Display,
    _xWin :: !Window,
    _capture :: !Drawable,
    _dim :: !(Int, Int)
  }
