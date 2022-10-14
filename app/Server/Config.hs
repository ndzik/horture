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

data Config = Config
  { twitchClientId :: !Text,
    twitchClientSecret :: !Text
  }
  deriving (Show)

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig fp = do
  bytes <- BSL.readFile fp
  return . decode @Config $ bytes

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''Config)
