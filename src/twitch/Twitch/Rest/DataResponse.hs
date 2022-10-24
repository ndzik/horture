module Twitch.Rest.DataResponse
  ( DataResponse (..),
  )
where

import Data.Aeson
import Data.Aeson.TH

newtype DataResponse v = DataResponse
  { dataresponseData :: v
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("dataresponse_" :: String)) . camelTo2 '_'} ''DataResponse)
