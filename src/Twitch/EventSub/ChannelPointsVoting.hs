module Twitch.EventSub.ChannelPointsVoting (ChannelPointsVoting (..)) where

data ChannelPointsVoting = ChannelPointsVoting
  { channelpointsvotingIsEnabled :: !Bool,
    channelpointsvotingAmountPerVote :: !Int
  } deriving Show
