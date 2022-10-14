module Main (main) where

import Data.Semigroup ((<>))
import Horture.Authorize
import Horture.CommandCenter.CommandCenter
import Options.Applicative

-- | Horture-Client:
-- This is the frontend client for horture.
--
--    $ horture-client --config <path-to-config>
-- executes the client itself, which will allow selecting a window to horture
-- and provide auxiliary information on the commandline. Default config is
-- located at searched for in `~/.config/horture/config.json`.
--
--    $ horture-client --authorize
-- will allow you to authorize horture to access your necessary twitch scopes.
-- It will also create a `config.json` file which is needed for the
-- `horture-client` to set up a connection to the horture server. The server
-- itself is needed because of Twitch's requirements on eventhook
-- subscriptions. We use subscriptions to be notified when viewers use, e.g.
-- channel redemptions, to trigger an event in the horture application.
-- The authorization uses an adaption of the credential flow below:
-- https://dev.twitch.tv/docs/authentication/getting-tokens-oauth/#client-credentials-grant-flow
main :: IO ()
main = execParser opts >>= handleParams
  where
    opts = info (cmdParser <**> helper)
                ( fullDesc
                    <> progDesc "Horture Client used to authorize and manage channel redemptions for twitch chat interactivity"
                    <> header "Horture-Client"
                )

data HortureParams = HortureParams
  { _config :: !FilePath,
    _authorize :: !Bool
  }
  deriving (Show)

cmdParser :: Parser HortureParams
cmdParser =
  HortureParams
    <$> strOption
      ( long "config"
          <> metavar "PATH-TO-CONFIG"
          <> help "Path to horture config file."
          <> showDefault
          <> value "~/.config/horture/config.json"
      )
    <*> switch
      ( long "authorize"
          <> short 'a'
          <> help "Authorize horture with for your twitch account"
      )

handleParams :: HortureParams -> IO ()
handleParams (HortureParams fp False) = runCommandCenter
handleParams (HortureParams fp True) = authorize fp
