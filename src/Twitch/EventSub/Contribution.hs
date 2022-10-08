module Twitch.EventSub.Contribution (LastContribution (..), TopContribution (..)) where

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
