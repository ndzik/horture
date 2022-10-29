module Horture.Server.Reader where

import Control.Lens
import Horture.Server.Config
import Control.Concurrent.Chan.Synchronous
import Data.ByteString
import qualified Twitch.EventSub.Notification as Twitch
import Katip

data HortureServerEnv = HortureServerEnv
  { _conf :: !HortureServerConfig,
    _notificationChan :: !(Chan Twitch.EventNotification),
    _secret :: !ByteString,
    _logEnv :: !LogEnv,
    _logContext :: !LogContexts,
    _logNamespace :: !Namespace
  }

makeLenses ''HortureServerEnv
