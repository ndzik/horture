module Horture.Server.Config  where

import Network.Wai.Handler.Warp
import Servant.Client.Core (BaseUrl (..))
import Control.Lens
import Data.Text (Text)

data HortureServerConfig = HortureServerConfig
  { _port :: !Port,
    _callback :: !BaseUrl,
    _certFile :: !(Maybe FilePath),
    _keyFile :: !(Maybe FilePath),
    _appClientId :: !Text,
    _appToken :: !Text
  }
  deriving (Show)

makeLenses ''HortureServerConfig
