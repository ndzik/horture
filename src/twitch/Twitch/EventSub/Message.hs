module Twitch.EventSub.Message (Message (..), Emote (..)) where

import Data.Aeson
import Data.Aeson.TH
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

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("message_" :: String)) . camelTo2 '_'} ''Message)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("emote_" :: String)) . camelTo2 '_'} ''Emote)
