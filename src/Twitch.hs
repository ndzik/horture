module Twitch () where

import Data.Text (Text)

-- TODO: EventSub
-- Need channel:manage:redemptions authorization for Channelredemption
-- endpoint.
--
-- Use
-- https://dev.twitch.tv/docs/eventsub/handling-webhook-events#simple-nodejs-example
-- as a blueprint.
--
-- For Haskell we will use Warp:
-- https://wiki.haskell.org/Web/Servers#Warp

data NotificationReq = NotificationReq
  { notificationreq_notification :: Text,
    notificationreq_webhook_callback_verification :: Text,
    notificationreq_revocation :: Text
  }
  deriving (Show)

data SubscriptionMsg = SubscriptionMsg
  { subscriptionmsg_subscription :: Subscription,
    subscriptionmsg_transport :: Transport,
    subscriptionmsg_created_at :: Text
  }
  deriving (Show)

data Subscription = Subscription
  { subscription_id :: Text,
    subscription_status :: Status,
    subscription_type :: SubType,
    subscription_version :: Text,
    subscription_cost :: Int,
    subscription_condition :: Condition,
    subscription_created_at :: Text
  }
  deriving (Show)

data SubType = ChannelPointsCustomRewardRedemption

instance Show SubType where
  show ChannelPointsCustomRewardRedemption = "channel.channel_points_custom_reward_redemption.add"

data Status
  = Enabled
  | Disabled
  deriving (Show)

-- TODO: Make this typesafe by associating SubTypes with their condition
-- structure.
data Condition = Condition
  { condition_broadcaster_user_id :: Text,
    condition_reward_id :: Text
  }
  deriving (Show)

data Transport = Transport
  { transport_method :: Method,
    transport_callback :: Text,
    transport_secret :: Text
  }
  deriving (Show)

data Method = WebHook

instance Show Method where
  show WebHook = "webhook"

data ChannelRedemptionPayload = ChannelRedemptionPayload
  { channel_redemption_payload_subscription :: Subscription,
    channel_redemption_payload_event :: Event
  }
  deriving (Show)

data Event = Event
  { event_id :: Text,
    event_broadcaster_user_id :: Text,
    event_broadcaster_user_name :: Text,
    event_user_id :: Text,
    event_user_login :: Text,
    event_user_name :: Text,
    event_user_input :: Text,
    event_status :: EventStatus,
    event_reward :: Reward,
    redeemed_at :: Text
  }
  deriving (Show)

data EventStatus = Unfulfilled | Unknown | Fulfilled | Canceled deriving (Show)

data Reward = Reward
  { reward_id :: Text,
    reward_title :: Text,
    reward_cost :: Int,
    reward_prompt :: Text
  }
  deriving (Show)
