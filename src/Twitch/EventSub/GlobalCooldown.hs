module Twitch.EventSub.GlobalCooldown (GlobalCooldown (..)) where

import Data.Aeson
import Data.Aeson.TH

data GlobalCooldown = GlobalCooldown
  { globalcooldownIsEnabled :: !Bool,
    globalcooldownSeconds :: !Int
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("globalcooldown_" :: String)) . camelTo2 '_'} ''GlobalCooldown)
