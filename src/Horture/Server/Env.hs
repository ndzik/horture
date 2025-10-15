module Horture.Server.Env where

import Control.Concurrent (MVar, ThreadId)
import Control.Concurrent.Chan.Synchronous (Chan)
import Control.Concurrent.STM (TVar)
import Control.Lens
import Data.Text (Text)
import Horture.Event
import Horture.Server.Protocol
import Network.WebSockets.Connection

data WSAppEnv = WSAppEnv
  { _wsAppEnvLogChan :: Chan Text,
    _wsAppEnvWriterPumpChan :: Chan CCReply,
    _wsAppEnvMV :: MVar Text,
    _wsAppEnvEnabledTVar :: TVar Bool,
    _wsAppEnvFPSTVar :: TVar Int,
    _wsAppEnvEvChan :: Chan Event,
    _wsAppEnvHortureTID :: ThreadId
  }

data WSClientEnv = WSClientEnv
  { _wsAppEnvWorkerTIDs :: TVar [ThreadId],
    _wsAppEnvConn :: Connection
  }

data ServerEnv = ServerEnv
  { _serverEnvClients :: TVar [Bool]
  }

makeLenses ''WSAppEnv
makeLenses ''WSClientEnv
