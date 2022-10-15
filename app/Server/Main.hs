{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Config
import Data.Aeson (decode)
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Semigroup ((<>))
import Horture.CommandCenter.CommandCenter
import Horture.Path
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Options.Applicative.Common (runParser)
import Servant.Client
import System.Exit (exitFailure)
import Twitch.Rest

-- | Handling twitch:
--   We let the front-end handle `authorization` for user & app tokens. When a
--   user wants to use this server to receive webhook events over websocket, he
--   will have to transmit his credentials after authorization with twitch to
--   the `/ws` endpoint. This ensures, that only a valid user can query his own
--   events, because only an authorized user can provide the necessary
--   credentials to set up eventhooks for a channel he is allowed to managed in
--   the specified scope.
--
--   Horture-Client calls `/ws` endpoint and builds a WS connection. Either the
--   initial request or the first payload received over the WS connection has
--   to contain necessary credentials.
--
--   Upon receival, Horture-Server exposes an endpoint `/<twitch-name>/webhook`
--   for twitch to push events to.
--
--   As long as the websocket connection is alive, the `/<twitch-name>/webhook`
--   endpoint is served. Upon a disconnect, Horture-Server will cancel its
--   subscriptions for events for good measures.
main :: IO ()
main = execParser opts >>= main'
  where
    opts =
      info
        (cmdParser <**> helper)
        ( fullDesc
            <> progDesc "Horture Relay Server handling Twitch webhooks and forwarding them over a WS connection."
            <> header "Horture-Relay-Server"
        )

main' :: ServerParams -> IO ()
main' (ServerParams cf _db) = do
  Config {twitchClientId, twitchClientSecret, twitchAuthorizationEndpoint} <-
    resolvePath cf >>= parseConfig >>= \case
      Nothing -> print "Config file ill-formatted or not available" >> exitFailure
      Just cfg -> return cfg
  mgr <-
    newManager =<< case baseUrlScheme twitchAuthorizationEndpoint of
      Https -> return tlsManagerSettings
      Http -> return defaultManagerSettings
  let TwitchTokenClient {getAppAccessToken} = twitchTokenClient
      clientEnv = mkClientEnv mgr twitchAuthorizationEndpoint
  res <- runClientM (getAppAccessToken (ClientCredentialRequest twitchClientId twitchClientSecret)) clientEnv
  creds <- case res of
    Left err -> print err >> exitFailure
    Right r -> return r
  print creds

data ServerParams = ServerParams
  { _config :: !FilePath,
    _debug :: !Bool
  }
  deriving (Show)

cmdParser :: Parser ServerParams
cmdParser =
  ServerParams
    <$> strOption
      ( long "config"
          <> metavar "PATH-TO-CONFIG"
          <> help "Path to server config file."
          <> showDefault
          <> value "~/.config/horture/server_config.json"
      )
    <*> switch
      ( long "debug"
          <> short 'd'
          <> help "Server debug mode"
      )
