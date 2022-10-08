module Twitch.EventSub.Choices (Choice (..)) where

import Data.Text (Text)

data Choice = Choice
  { choiceId :: !Text,
    choiceTitle :: !Text,
    choiceBitsVotes :: !Int,
    choiceChannelPointsVotes :: !Int,
    choiceVotes :: !Int
  }
  deriving (Show)
