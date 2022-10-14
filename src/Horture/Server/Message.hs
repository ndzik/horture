module Horture.Server.Message (HortureClientMessage (..)) where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.WebSockets

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

$(deriveJSON defaultOptions ''HortureClientMessage)
