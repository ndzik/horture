module Twitch.EventSub.Reward (MaxPerStream (..), MaxPerUserPerStream (..)) where

data MaxPerStream = MaxPerStream
  { maxperstreamIsEnabled :: !Bool,
    maxperstreamValue :: !Int
  }
  deriving (Show)

data MaxPerUserPerStream = MaxPerUserPerStream
  { maxperuserperstreamIsEnabled :: !Bool,
    maxperuserperstreamValue :: !Int
  }
  deriving (Show)
