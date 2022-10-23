module Twitch.EventSub.Transport (Transport(..)) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

-- EventSub transport method.
data Transport = Transport
  { transportMethod :: !Text,
    transportCallback :: !Text,
    transportSecret :: !(Maybe Text)
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("transport_" :: String)) . camelTo2 '_'} ''Transport)
