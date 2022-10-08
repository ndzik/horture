module Twitch.EventSub.Poll (PollStatus(..)) where

import Data.Aeson
import Data.Aeson.TH

data PollStatus = Completed | Archived | Terminated deriving Show

$(deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''PollStatus)
