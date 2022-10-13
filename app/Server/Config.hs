{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config (..),
    parseConfig,
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BSL

data Config = Config
  { _clientId :: !Text,
    _clientSecret :: !Text
  }
  deriving (Show)

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig fp = do
  bytes <- BSL.readFile fp
  return . decode @Config $ bytes

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config)
