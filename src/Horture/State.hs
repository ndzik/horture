module Horture.State where

import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens.TH
import Data.RingBuffer
import Data.Text (Text)
import qualified Data.Vector as V
import Foreign (Ptr)
import Graphics.Rendering.OpenGL hiding (get)
import qualified Graphics.UI.GLFW as GLFW
import Horture.Audio.Audio
import Horture.Audio.Recorder
import Horture.Event
import Horture.Program
import Horture.RenderBridge

data HortureEnv hdl = HortureEnv
  { -- READONLY
    _screenProg :: !HortureScreenProgram,
    _dynamicImageProg :: !HortureDynamicImageProgram,
    _backgroundProg :: !HortureBackgroundProgram,
    _fontProg :: !HortureFontProgram,
    _audioEnv :: !AudioPlayerEnv,
    _eventChan :: !(Chan Event),
    _logChan :: !(Maybe (Chan Text)),
    _glWin :: !GLFW.Window,
    _backgroundColor :: !(Color4 Float),
    -- WRITEABLE
    _envHandle :: !(TVar hdl),
    _renderBridgeCtx :: !(TVar (Ptr RB)),
    _audioRecording :: !(MVar AudioRecorderEnv),
    _audioBandState :: !(TVar AllBands),
    _audioStorage :: !(TVar (Maybe FFTSnapshot)),
    _audioState :: !(TVar AudioPlayerState),
    _frameCounter :: !(TVar Int),
    _mvgAvg :: !(TVar (RingBuffer V.Vector FFTSnapshot)),
    _dim :: !(TVar (GLsizei, GLsizei)),
    _eventList :: !(TVar (RingBuffer V.Vector PastEvent))
  }

makeLenses ''HortureEnv
