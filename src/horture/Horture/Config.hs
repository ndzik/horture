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
    twitchClientSecret :: !(Maybe Text),
    twitchAuthorizationEndpoint :: !BaseUrl,
    twitchApiEndpoint :: !BaseUrl,
    -- | mockUserId is only used when testing against a local twitch-api using
    -- `twitch-cli`.
    mockUserId :: !(Maybe Text),
    twitchAuthToken :: !(Maybe Text),
    hortureWsEndpoint :: !(Maybe BaseUrl),
    hortureCost :: !Int,
    gifDirectory :: !FilePath,
    debugDelayMs :: !Int
  }
  deriving (Show)

defaultDebugDelay :: Int
defaultDebugDelay = 1_000_000

defaultHortureCost :: Int
defaultHortureCost = 50

instance Default Config where
  def =
    Config
      { twitchClientId = "invalid-client-id",
        twitchClientSecret = Nothing,
        twitchAuthorizationEndpoint = BaseUrl Https "localhost" 8080 "auth/authorize",
        twitchApiEndpoint = BaseUrl Http "localhost" 8080 "mock",
        twitchAuthToken = Nothing,
        hortureWsEndpoint = Nothing,
        mockUserId = Nothing,
        hortureCost = defaultHortureCost,
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
      <*> ( o .:? "twitch_client_secret" >>= \case
              Just "" -> return Nothing
              v -> return v
          )
      <*> o .: "twitch_authorization_endpoint"
      <*> o .: "twitch_api_endpoint"
      <*> ( o .:? "mock_user_id" >>= \case
              Just "" -> return Nothing
              v -> return v
          )
      <*> ( o .:? "twitch_auth_token" >>= \case
              Just "" -> return Nothing
              v -> return v
          )
      <*> (o .:? "horture_ws_endpoint")
      <*> ( o .:? "horture_cost" >>= \case
              Just v -> return v
              _otherwise -> return defaultHortureCost
          )
      <*> o .: "gif_directory"
      <*> ( o .:? "debug_delay_ms" >>= \case
              Just v -> return v
              _otherwise -> return defaultDebugDelay
          )

$(deriveToJSON defaultOptions {fieldLabelModifier = camelTo2 '_', omitNothingFields = True} ''Config)
