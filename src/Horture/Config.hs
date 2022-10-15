-- | Horture client configuration.
module Horture.Config
  ( Config (..),
    parseHortureClientConfig,
  )
where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Text (Text)
import Servant.Client

data Config = Config
  { twitchClientId :: !Text,
    twitchAuthorizationEndpoint :: !BaseUrl,
    twitchApiEndpoint :: !BaseUrl,
    gifDirectory :: !FilePath
  }
  deriving (Show)

instance Default Config where
  def =
    Config
      { twitchClientId = "invalid-client-id",
        twitchAuthorizationEndpoint = BaseUrl Https "id.twitch.tv" 443 "oauth2/authorize",
        twitchApiEndpoint = BaseUrl Https "api.twitch.tv" 443 "",
        gifDirectory = "./gifs"
      }

parseHortureClientConfig :: FilePath -> IO (Maybe Config)
parseHortureClientConfig fp = do
  bytes <- BSL.readFile fp
  return . decode @Config $ bytes

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''Config)
