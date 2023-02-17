module Horture.CommandCenter.State
  ( CommandCenterState (..),
    ccEventChan,
    ccBrickEventChan,
    ccCapturedWin,
    ccControllerChans,
    ccLog,
    ccImages,
    ccHortureUrl,
    ccUserId,
    ccPreloadedSounds,
    ccLogList,
    ccPreloadedImages,
    ccRegisteredEffects,
    ccTIDsToClean,
    ccTimeout,
    ccFrameCounter,
    ccImagesList,
    ccEventBaseCost,
    ccCursorLocationName,
    ccEventSourceEnabled,
    ccDefaultFont,
    Name (..),
  )
where

import Brick.BChan
import Brick.Widgets.List (GenericList)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM (TVar)
import Control.Lens
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Horture.CommandCenter.Event
import Horture.Effect
import Horture.Event
import Horture.EventSource.Controller
import Horture.Loader.Asset
import qualified RingBuffers.Lifted as RingBuffer
import Servant.Client (BaseUrl)

data Name
  = MetaPort
  | AssetPort
  | LogPort
  deriving (Ord, Show, Eq)

data CommandCenterState = CCState
  { _ccEventChan :: !(Maybe (Chan Event)),
    _ccBrickEventChan :: !(BChan CommandCenterEvent),
    _ccCapturedWin :: !(Maybe String),
    _ccControllerChans :: !(Maybe (Chan EventControllerInput, Chan EventControllerResponse)),
    _ccLog :: !(RingBuffer.RingBuffer Text),
    _ccLogList :: ![Text],
    _ccImages :: ![FilePath],
    _ccImagesList :: !(GenericList Name [] FilePath),
    _ccHortureUrl :: !(Maybe BaseUrl),
    _ccEventSourceEnabled :: !(Maybe (TVar Bool)),
    _ccUserId :: !Text,
    _ccDefaultFont :: !(Maybe FilePath),
    _ccPreloadedImages :: ![(FilePath, Asset)],
    _ccPreloadedSounds :: ![(FilePath, Asset)],
    _ccRegisteredEffects :: !(Map.Map Text (Text, Effect)),
    _ccTIDsToClean :: ![ThreadId],
    _ccFrameCounter :: !(TVar Int),
    _ccCursorLocationName :: !Name,
    _ccEventBaseCost :: !Int,
    -- | Timeout in microseconds for events to be generated. Only works in
    -- DEBUG mode.
    _ccTimeout :: !Int
  }

makeLenses ''CommandCenterState
