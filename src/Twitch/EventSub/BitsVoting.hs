module Twitch.EventSub.BitsVoting (BitsVote (..)) where

data BitsVote = BitsVote
  { bitsvoteIsEnabled :: !Bool,
    bitsvoteAmountPerVote :: !Int
  } deriving Show
