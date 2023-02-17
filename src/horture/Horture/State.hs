module Horture.State where

import Control.Concurrent (MVar)
import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM
import Control.Lens.TH
import Data.Text (Text)
import Graphics.Rendering.OpenGL hiding (get)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Horture.Audio.Recorder
import Horture.Event
import Horture.Audio.Player.Protea
import Horture.Program
import RingBuffers.Lifted

data HortureStatic = HortureStatic
  { _screenProg :: !HortureScreenProgram,
    _dynamicImageProg :: !HortureDynamicImageProgram,
    _backgroundProg :: !HortureBackgroundProgram,
    _fontProg :: !HortureFontProgram,
    _audioEnv :: !AudioPlayerEnv,
    _eventChan :: !(Chan Event),
    _logChan :: !(Maybe (Chan Text)),
    _glWin :: !GLFW.Window,
    _backgroundColor :: !(Color4 Float)
  }

data HortureState hdl = HortureState
  { _envHandle :: !hdl,
    _audioRecording :: !(Maybe (MVar ())),
    _audioStorage :: !(TVar (Maybe FFTSnapshot)),
    _audioState :: !AudioPlayerState,
    _frameCounter :: !(TVar Int),
    _mvgAvg :: !(RingBuffer FFTSnapshot),
    _capture :: !(Maybe Drawable),
    _dim :: !(Int, Int),
    _eventList :: !(RingBuffer PastEvent)
  }

makeLenses ''HortureStatic
makeLenses ''HortureState
