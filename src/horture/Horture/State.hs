module Horture.State where

import Control.Concurrent.Chan.Synchronous
import Control.Lens.TH
import Data.Text (Text)
import Graphics.Rendering.OpenGL hiding (get)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Horture.Event
import Horture.Program

data HortureStatic = HortureStatic
  { _screenProg :: !HortureScreenProgram,
    _gifProg :: !HortureGifProgram,
    _planeVertexLocation :: !AttribLocation,
    _planeTexLocation :: !AttribLocation,
    _eventChan :: !(Chan Event),
    _logChan :: !(Maybe (Chan Text)),
    _glWin :: !GLFW.Window,
    _backgroundColor :: !(Color4 Float)
  }

data HortureState = HortureState
  { _display :: !Display,
    _xWin :: !Window,
    _capture :: !Drawable,
    _dim :: !(Int, Int)
  }

makeLenses ''HortureStatic
makeLenses ''HortureState
