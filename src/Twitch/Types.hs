{-# LANGUAGE TemplateHaskell #-}

module Twitch.Types
  ( NotificationReq (..),
    SubscriptionMsg (..),
    Subscription (..),
    Status (..),
    Condition (..),
    Transport (..),
    ChannelRedemptionPayload (..),
    Verification (..),
    Event (..),
    EventStatus (..),
    Reward (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Text (Text)

data Verification = Verification
  { verificationChallenge :: !Text,
    verificationSubscription :: !Subscription
  }
  deriving (Show)

data NotificationReq = NotificationReq
  { notificationreqNotification :: !Text,
    notificationreqWebhookCallbackVerification :: !Text,
    notificationreqRevocation :: !Text
  }
  deriving (Show)

data SubscriptionMsg = SubscriptionMsg
  { subscriptionmsgSubscription :: !Subscription,
    subscriptionmsgTransport :: !Transport,
    subscriptionmsgCreatedAt :: !Text
  }
  deriving (Show)

data Subscription = Subscription
  { subscriptionId :: !Text,
    subscriptionStatus :: !Text,
    subscriptionType :: !Text,
    subscriptionVersion :: !Text,
    subscriptionCost :: !Int,
    subscriptionCondition :: !Condition,
    subscriptionTransport :: !Transport,
    subscriptionCreatedAt :: !Text
  }
  deriving (Show)

data Status
  = Enabled
  | Disabled
  deriving (Show)

-- TODO: Make this typesafe by associating SubTypes with their condition
-- structure.
data Condition = Condition
  { conditionBroadcasterUserId :: !Text
  -- conditionRewardId :: !Text
  }
  deriving (Show)

data Transport = Transport
  { transportMethod :: !Text,
    transportCallback :: !Text,
    transportSecret :: !(Maybe Text)
  }
  deriving (Show)

-- data Method = WebHook
--
-- instance Show Method where
--   show WebHook = "webhook"

data ChannelRedemptionPayload = ChannelRedemptionPayload
  { channelRedemptionPayloadSubscription :: !Subscription,
    channelRedemptionPayloadEvent :: !Event
  }
  deriving (Show)

data Event = Event
  { eventId :: !Text,
    eventBroadcasterUserId :: !Text,
    eventBroadcasterUserName :: !Text,
    eventUserId :: !Text,
    eventUserLogin :: !Text,
    eventUserName :: !Text,
    eventUserInput :: !Text,
    eventStatus :: !EventStatus,
    eventReward :: !Reward,
    eventRedeemedAt :: !Text
  }
  deriving (Show)

data EventStatus = Unfulfilled | Unknown | Fulfilled | Canceled deriving (Show)

data Reward = Reward
  { rewardId :: !Text,
    rewardTitle :: !Text,
    rewardCost :: !Int,
    rewardPrompt :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length "verification_") . camelTo2 '_'} ''Verification)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length "subscription_") . camelTo2 '_'} ''Subscription)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length "transport_") . camelTo2 '_'} ''Transport)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length "condition_") . camelTo2 '_'} ''Condition)
$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Status)
