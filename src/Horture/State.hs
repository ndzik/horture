module Horture.State where

import Control.Concurrent (MVar)
import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM
import Control.Lens.TH
import Data.RingBuffer
import Data.Text (Text)
import qualified Data.Vector as V
-- import Graphics.X11
-- import Horture.Audio.Player.Protea

import Foreign (Ptr)
import Graphics.Rendering.OpenGL hiding (get)
import qualified Graphics.UI.GLFW as GLFW
import Horture.Audio.Recorder
import Horture.Event
import Horture.Program
import Horture.RenderBridge

data AudioPlayerEnv = AudioPlayerEnv

data AudioPlayerState = AudioPlayerState

data Drawable = Drawable

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
    _renderBridgeCtx :: !(Ptr RB),
    _audioRecording :: !(Maybe (MVar ())),
    _audioStorage :: !(TVar (Maybe FFTSnapshot)),
    _audioState :: !AudioPlayerState,
    _frameCounter :: !(TVar Int),
    _mvgAvg :: !(RingBuffer V.Vector FFTSnapshot),
    _capture :: !(Maybe Drawable),
    _dim :: !(TVar (GLsizei, GLsizei)),
    _eventList :: !(RingBuffer V.Vector PastEvent)
  }

makeLenses ''HortureStatic
makeLenses ''HortureState
