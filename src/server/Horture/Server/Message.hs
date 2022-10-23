module Horture.Server.Message (HortureClientMessage (..),
  HortureServerMessage(..)) where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.WebSockets
import qualified Twitch.EventSub.Event as Twitch

-- | HortureClientMessage are all messages pushed to the server by client.
data HortureClientMessage
  = HortureAuthorization
      { _userAccessToken :: !Text
      }
  | HortureUpdate
      { _userAccessToken :: !Text
      }
  | HortureGarbage

instance WebSocketsData HortureClientMessage where
  fromDataMessage (Network.WebSockets.Text bs _) = fromMaybe HortureGarbage (decode bs)
  fromDataMessage (Network.WebSockets.Binary bs) = fromMaybe HortureGarbage (decode bs)
  fromLazyByteString bs = fromMaybe HortureGarbage (decode bs)
  toLazyByteString = encode

-- | HortureServerMessage are all messages pushed by the server to clients.
data HortureServerMessage
  = HortureEventSub !Twitch.Event
  | HortureServerGarbage

instance WebSocketsData HortureServerMessage where
  fromDataMessage (Network.WebSockets.Text bs _) = fromMaybe HortureServerGarbage (decode bs)
  fromDataMessage (Network.WebSockets.Binary bs) = fromMaybe HortureServerGarbage (decode bs)
  fromLazyByteString bs = fromMaybe HortureServerGarbage (decode bs)
  toLazyByteString = encode

$(deriveJSON defaultOptions ''HortureClientMessage)
$(deriveJSON defaultOptions ''HortureServerMessage)
