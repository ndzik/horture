{-# LANGUAGE TemplateHaskell #-}

module Twitch.Types
  ( NotificationReq (..),
    Subscription (..),
    Status (..),
    Transport (..),
    ChannelRedemptionNotification (..),
    WebhookRequest (..),
    RewardRedemptionCondition (..),
    Verification (..),
    RedemptionRewardEvent (..),
    EventStatus (..),
    Reward (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Text (Text)

data NotificationEvent c = NotificationEvent
  { notificationeventSubscription :: !(Subscription c),
    notificationeventEvent :: !RedemptionRewardEvent
  }
  deriving (Show)

data Verification c = Verification
  { verificationChallenge :: !Text,
    verificationSubscription :: !(Subscription c)
  }
  deriving (Show)

data NotificationReq = NotificationReq
  { notificationreqNotification :: !Text,
    notificationreqWebhookCallbackVerification :: !Text,
    notificationreqRevocation :: !Text
  }
  deriving (Show)

-- Subscription is the webhook subscription event type received from twitch
-- when requesting a webhook subscription.
data Subscription c = Subscription
  { subscriptionId :: !Text,
    subscriptionType :: !Text,
    subscriptionVersion :: !Text,
    subscriptionStatus :: !Status,
    subscriptionCost :: !Int,
    subscriptionCondition :: !c,
    subscriptionTransport :: !Transport,
    subscriptionCreatedAt :: !Text
  }
  deriving (Show)

data Status
  = Enabled
  | Disabled
  deriving (Show)

-- TODO: Could we use TypeLits here to statically fix the request type and
-- still generate everything using TH?
data WebhookRequest c = MkWebhookRequest
  { mkwebhookrequestType :: !Text,
    mkwebhookrequestVersion :: !Text,
    mkwebhookrequestCondition :: !c,
    mkwebhookrequestTransport :: !Transport
  }
  deriving (Show)

-- type: "channel.channel_points_custom_reward_redemption.add"
data RewardRedemptionCondition = RewardRedemptionCondition
  { rewardredemptionconditionBroadcasterUserId :: !Text,
    rewardredemptionconditionRewardId :: !(Maybe Text)
  }
  deriving (Show)

data Transport = Transport
  { transportMethod :: !Text,
    transportCallback :: !Text,
    transportSecret :: !(Maybe Text)
  }
  deriving (Show)

data ChannelRedemptionNotification c = ChannelRedemptionNotification
  { channelredemptionnotificationSubscription :: !(Subscription c),
    channelredemptionnotificationEvent :: !RedemptionRewardEvent
  }
  deriving (Show)

data RedemptionRewardEvent = RedemptionRewardEvent
  { redemptionrewardeventId :: !Text,
    redemptionrewardeventBroadcasterUserId :: !Text,
    redemptionrewardeventBroadcasterUserLogin :: !Text,
    redemptionrewardeventBroadcasterUserName :: !Text,
    redemptionrewardeventUserId :: !Text,
    redemptionrewardeventUserLogin :: !Text,
    redemptionrewardeventUserName :: !Text,
    redemptionrewardeventUserInput :: !Text,
    redemptionrewardeventStatus :: !EventStatus,
    redemptionrewardeventReward :: !Reward,
    redemptionrewardeventRedeemedAt :: !Text
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
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length "mkwebhookrequest_") . camelTo2 '_'} ''WebhookRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length "channelredemptionnotification_") . camelTo2 '_'} ''ChannelRedemptionNotification)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length "rewardredemptioncondition_") . camelTo2 '_'} ''RewardRedemptionCondition)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length "redemptionrewardevent_") . camelTo2 '_'} ''RedemptionRewardEvent)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length "reward_") . camelTo2 '_'} ''Reward)
$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''EventStatus)
$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Status)
