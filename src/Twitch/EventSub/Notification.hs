{-# LANGUAGE DataKinds #-}

module Twitch.EventSub.Notification
  ( EventNotification (..),
   ChallengeNotification (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Twitch.EventSub.Event as Twitch
import Twitch.EventSub.Response

data EventNotification = EventNotification
  { eventnotificationSubscription :: !ResponseObject,
    eventnotificationEvent :: !Twitch.Event
  }
  deriving (Show)

instance FromJSON EventNotification where
  parseJSON v =
    withObject
      "EventNotification"
      ( \o -> do
          ro <- o .: "subscription"
          ev <- parseJSON v
          return $ EventNotification ro ev
      )
      v

data ChallengeNotification = ChallengeNotification
  { challengenotificationChallenge :: !Text,
    challengenotificationSubscription :: !ResponseObject
  }
  deriving (Show)

$(deriveToJSON defaultOptions {fieldLabelModifier = drop (length ("eventnotification_" :: String)) . camelTo2 '_'} ''EventNotification)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("challengenotification_" :: String)) . camelTo2 '_'} ''ChallengeNotification)
