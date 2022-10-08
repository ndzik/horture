module Twitch.EventSub.Reward (MaxPerStream (..), MaxPerUserPerStream (..)) where

import Data.Aeson
import Data.Aeson.TH

data MaxPerStream = MaxPerStream
  { maxperstreamIsEnabled :: !Bool,
    maxperstreamValue :: !Int
  }
  deriving (Show)

data MaxPerUserPerStream = MaxPerUserPerStream
  { maxperuserperstreamIsEnabled :: !Bool,
    maxperuserperstreamValue :: !Int
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("maxperstream_" :: String)) . camelTo2 '_'} ''MaxPerStream)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("maxperuserperstream_" :: String)) . camelTo2 '_'} ''MaxPerUserPerStream)
