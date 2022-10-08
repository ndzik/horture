module Twitch.EventSub.Outcomes (Outcome (..), TopPredictor (..)) where

import Data.Text (Text)

data Outcome = Outcome
  { outcomesId :: !Text,
    outcomesTitle :: !Text,
    outcomesColor :: !Text,
    outcomesUsers :: !Int,
    outcomesChannelPoints :: !Int,
    outcomesTopPredictors :: ![TopPredictor]
  }
  deriving (Show)

data TopPredictor = TopPredictor
  { toppredictorUserId :: !Text,
    toppredictorUserLogin :: !Text,
    toppredictorUserName :: !Text,
    toppredictorChannelPointsWon :: !Int,
    toppredictorChannelPointsUsed :: !Int
  }
  deriving (Show)
