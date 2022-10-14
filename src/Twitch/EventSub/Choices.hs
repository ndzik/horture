module Twitch.EventSub.Choices (Choice (..)) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH

data Choice = Choice
  { choiceId :: !Text,
    choiceTitle :: !Text,
    choiceBitsVotes :: !Int,
    choiceChannelPointsVotes :: !Int,
    choiceVotes :: !Int
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("choice_" :: String)) . camelTo2 '_'} ''Choice)
