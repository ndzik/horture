module Twitch.EventSub.BitsVoting (BitsVote (..)) where

import Data.Aeson
import Data.Aeson.TH

data BitsVote = BitsVote
  { bitsvoteIsEnabled :: !Bool,
    bitsvoteAmountPerVote :: !Int
  } deriving Show

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("bitsvote_" :: String)) . camelTo2 '_'} ''BitsVote)
