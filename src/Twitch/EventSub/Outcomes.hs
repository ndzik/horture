module Twitch.EventSub.Outcomes (Outcome (..), TopPredictor (..)) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

data Outcome = Outcome
  { outcomeId :: !Text,
    outcomeTitle :: !Text,
    outcomeColor :: !Text,
    outcomeUsers :: !Int,
    outcomeChannelPoints :: !Int,
    outcomeTopPredictors :: ![TopPredictor]
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

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("outcome_" :: String)) . camelTo2 '_'} ''Outcome)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("toppredictor_" :: String)) . camelTo2 '_'} ''TopPredictor)
