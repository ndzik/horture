module Horture.Server.Config (HortureServerConfig (..)) where

import Network.Wai.Handler.Warp
import Servant.Client.Core (BaseUrl (..))

data HortureServerConfig = HortureServerConfig
  { _port :: !Port,
    _callback :: !BaseUrl
  }
  deriving (Show)
