module Twitch.EventSub.Message (Message (..), Emote(..)) where

import Data.Text (Text)

data Message = Message
  { messageText :: !Text,
    messageEmotes :: ![Emote]
  }
  deriving (Show)

data Emote = Emote
  { emoteBegin :: !Int,
    emoteEnd :: !Int,
    emoteId :: !Text
  }
  deriving (Show)
