module Twitch.EventSub.Contribution
  ( LastContribution (..),
    TopContribution (..),
    ContributionType(..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

data TopContribution = TopContribution
  { topcontributionUserId :: !Text,
    topcontributionUserLogin :: !Text,
    topcontributionUserName :: !Text,
    topcontributionType :: !ContributionType,
    topcontributionTotal :: !Int
  }
  deriving (Show)

data LastContribution = LastContribution
  { lastcontributionUserId :: !Text,
    lastcontributionUserLogin :: !Text,
    lastcontributionUserName :: !Text,
    lastcontributionType :: !ContributionType,
    lastcontributionTotal :: !Int
  }
  deriving (Show)

data ContributionType = Bits | Subscription deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("topcontribution_" :: String)) . camelTo2 '_'} ''TopContribution)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("lastcontribution_" :: String)) . camelTo2 '_'} ''LastContribution)
$(deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''ContributionType)
