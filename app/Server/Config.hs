{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Config
  ( Config (..),
    parseConfig,
  )
where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Network.Wai.Handler.Warp
import Servant.Client (BaseUrl)

data Config = Config
  { twitchClientId :: !Text,
    twitchClientSecret :: !Text,
    twitchAuthorizationEndpoint :: !BaseUrl,
    twitchApiEndpoint :: !BaseUrl,
    serverPort :: !Port,
    certFile :: !(Maybe FilePath),
    keyFile :: !(Maybe FilePath),
    twitchResponseCallback :: !BaseUrl
  }
  deriving (Show)

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig fp = do
  bytes <- BSL.readFile fp
  return . decode @Config $ bytes

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''Config)
