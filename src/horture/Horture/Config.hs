{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

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
    twitchAuthToken :: !(Maybe Text),
    hortureWsEndpoint :: !(Maybe BaseUrl),
    gifDirectory :: !FilePath,
    debugDelayMs :: !Int
  }
  deriving (Show)

defaultDebugDelay :: Int
defaultDebugDelay = 1_000_000

instance Default Config where
  def =
    Config
      { twitchClientId = "invalid-client-id",
        twitchAuthorizationEndpoint = BaseUrl Https "id.twitch.tv" 443 "oauth2/authorize",
        twitchApiEndpoint = BaseUrl Https "api.twitch.tv" 443 "",
        twitchAuthToken = Nothing,
        hortureWsEndpoint = Nothing,
        gifDirectory = "./gifs",
        debugDelayMs = defaultDebugDelay
      }

parseHortureClientConfig :: FilePath -> IO (Maybe Config)
parseHortureClientConfig fp = do
  bytes <- BSL.readFile fp
  return . decode @Config $ bytes

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config <$> o .: "twitch_client_id"
      <*> o .: "twitch_authorization_endpoint"
      <*> o .: "twitch_api_endpoint"
      <*> ( o .:? "twitch_auth_token" >>= \case
              Just "" -> return Nothing
              v -> return v
          )
      <*> (o .:? "horture_ws_endpoint")
      <*> o .: "gif_directory"
      <*> (o .:? "debug_delay_ms" >>= \case
              Just v -> return v
              _otherwise -> return defaultDebugDelay)

$(deriveToJSON defaultOptions {fieldLabelModifier = camelTo2 '_', omitNothingFields = True} ''Config)
