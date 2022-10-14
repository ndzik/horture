module Twitch.EventSub.ChannelPointsVoting (ChannelPointsVoting (..)) where

import Data.Aeson
import Data.Aeson.TH

data ChannelPointsVoting = ChannelPointsVoting
  { channelpointsvotingIsEnabled :: !Bool,
    channelpointsvotingAmountPerVote :: !Int
  } deriving Show

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelpointsvoting_" :: String)) . camelTo2 '_'} ''ChannelPointsVoting)
