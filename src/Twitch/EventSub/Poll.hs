module Twitch.EventSub.Poll (PollStatus(..)) where

data PollStatus = Completed | Archived | Terminated deriving Show
