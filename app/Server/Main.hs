{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Config
import Data.Functor ((<&>))
import Horture.Server.Config
import Horture.Server.Server
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Servant.Client
import System.Directory (getHomeDirectory)
import System.Exit (exitFailure)
import Twitch.Rest

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
  Config {..} <-
    resolvePath cf >>= parseConfig >>= \case
      Nothing -> print "Config file ill-formatted or not available" >> exitFailure
      Just cfg -> return cfg
  mgr <-
    newManager =<< case baseUrlScheme twitchAuthorizationEndpoint of
      Https -> return tlsManagerSettings
      Http -> return defaultManagerSettings
  let TwitchTokenClient {getAppAccessToken} = twitchTokenClient
      clientEnv = mkClientEnv mgr twitchAuthorizationEndpoint
      ccr = ClientCredentialRequest twitchClientId twitchClientSecret
  res <- runClientM (getAppAccessToken ccr) clientEnv
  ClientCredentialResponse aat _ _ <- case res of
    Left err -> print err >> exitFailure
    Right r -> return r
  print "Successfully retrieved AppAccessToken from Twitch"
  runHorture (HortureServerConfig serverPort twitchResponseCallback certFile keyFile twitchClientId aat)

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

resolvePath :: FilePath -> IO FilePath
resolvePath ('~' : rs) = getHomeDirectory <&> (++ rs)
resolvePath rs = return rs
