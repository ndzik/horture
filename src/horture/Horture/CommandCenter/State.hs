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
    ccImagesList,
    ccEventBaseCost,
    ccCursorLocationName,
    ccEventSourceEnabled,
    ccDefaultFont,
    Name (..),
  )
where

import Brick.BChan
import Brick.Widgets.List (GenericList, list)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM (TVar)
import Control.Lens
import Data.Default
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Horture.CommandCenter.Event
import Horture.Effect
import Horture.Event
import Horture.EventSource.Controller
import Horture.Loader.Asset
import Servant.Client (BaseUrl)
import RingBuffers.Lifted

data Name
  = MetaPort
  | AssetPort
  | LogPort
  deriving (Ord, Show, Eq)

data CommandCenterState = CCState
  { _ccEventChan :: !(Maybe (Chan Event)),
    _ccBrickEventChan :: !(Maybe (BChan CommandCenterEvent)),
    _ccCapturedWin :: !(Maybe String),
    _ccControllerChans :: !(Maybe (Chan EventControllerInput, Chan EventControllerResponse)),
    _ccLog :: !(Maybe (RingBuffer Text)),
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
    _ccCursorLocationName :: !Name,
    _ccEventBaseCost :: !Int,
    -- | Timeout in microseconds for events to be generated. Only works in
    -- DEBUG mode.
    _ccTimeout :: !Int
  }

instance Default CommandCenterState where
  def =
    CCState
      { _ccEventChan = Nothing,
        _ccBrickEventChan = Nothing,
        _ccCapturedWin = Nothing,
        _ccControllerChans = Nothing,
        _ccHortureUrl = Nothing,
        _ccDefaultFont = Nothing,
        _ccRegisteredEffects = Map.empty,
        _ccUserId = "",
        _ccEventSourceEnabled = Nothing,
        _ccLog = Nothing,
        _ccLogList = [],
        _ccImages = [],
        _ccImagesList = list AssetPort [] 1,
        _ccPreloadedImages = [],
        _ccPreloadedSounds = [],
        _ccTIDsToClean = [],
        _ccCursorLocationName = LogPort,
        _ccEventBaseCost = 50,
        _ccTimeout = 1_000_000
      }

makeLenses ''CommandCenterState
