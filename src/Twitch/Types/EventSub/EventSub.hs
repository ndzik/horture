{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitch.Types.EventSub.EventSub (WebhookRequest (..), Transport (..)) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Twitch.Types.EventSub.Condition as Condition

data WebhookRequest = WebhookRequest
  { webhookrequestVersion :: !Text,
    webhookrequestCondition :: !Condition.Condition,
    webhookrequestTransport :: !Transport
  }
  deriving (Show)

instance FromJSON WebhookRequest where
  parseJSON v =
    withObject
      "WebhookRequest"
      ( \o ->
          WebhookRequest <$> o .: "version" <*> parseJSON v <*> o .: "transport"
      )
      v

instance ToJSON WebhookRequest where
  toJSON (WebhookRequest v c t) = object ["type" .= conditionTag c, "version" .= v, "condition" .= c, "transport" .= t]

conditionTag :: Condition.Condition -> Text
conditionTag Condition.ChannelUpdate {} = "channel.update"
conditionTag Condition.ChannelFollow {} = "channel.follow"
conditionTag Condition.ChannelSubscribe {} = "channel.subscribe"
conditionTag Condition.ChannelSubscriptionEnd {} = "channel.subscription.end"
conditionTag Condition.ChannelSubscriptionGift {} = "Channel.subscription.gift"
conditionTag Condition.ChannelCheer {} = "channel.cheer"
conditionTag Condition.ChannelRaid {} = "channel.raid"
conditionTag Condition.ChannelBan {} = "channel.ban"
conditionTag Condition.ChannelUnban {} = "channel.unban"
conditionTag Condition.ChannelModeratorAdd {} = "channel.moderator.add"
conditionTag Condition.ChannelModeratorRemove {} = "channel.moderator.remove"
conditionTag Condition.ChannelPointsCustomRewardAdd {} = "channel.channel_points_custom_reward.add"
conditionTag Condition.ChannelPointsCustomRewardUpdate {} = "channel.channel_points_custom_reward.update"
conditionTag Condition.ChannelPointsCustomRewardRemove {} = "channel.channel_points_custom_reward.remove"
conditionTag Condition.ChannelPointsCustomRewardRedemptionAdd {} = "channel.channel_points_custom_reward_redemption.add"
conditionTag Condition.ChannelPointsCustomRewardRedemptionUpdate {} = "channel.channel_points_custom_reward_redemption.update"
conditionTag Condition.ChannelPollBegin {} = "channel.poll.begin"
conditionTag Condition.ChannelPollProgress {} = "channel.poll.progress"
conditionTag Condition.ChannelPollEnd {} = "channel.poll.end"
conditionTag Condition.ChannelPredictionBegin {} = "channel.prediction.begin"
conditionTag Condition.ChannelPredictionProgress {} = "channel.prediction.progress"
conditionTag Condition.ChannelPredictionLock {} = "channel.prediction.lock"
conditionTag Condition.ChannelPredictionEnd {} = "channel.prediction.end"
conditionTag Condition.DropEntitlementGrant {} = "drop.entitlement.grant"
conditionTag Condition.ExtensionBitsTransactionCreate {} = "extension.bits_transaction.create"
conditionTag Condition.GoalBegin {} = "channel.goal.begin"
conditionTag Condition.GoalProgress {} = "channel.goal.progress"
conditionTag Condition.GoalEnd {} = "channel.goal.end"
conditionTag Condition.HypeTrainBegin {} = "channel.hype_train.begin"
conditionTag Condition.HypeTrainProgress {} = "channel.hype_train.progress"
conditionTag Condition.HypeTrainEnd {} = "channel.hype_train.end"
conditionTag Condition.StreamOnline {} = "stream.online"
conditionTag Condition.StreamOffline {} = "stream.offline"
conditionTag Condition.AuthorizationGrant {} = "user.authorization.grant"
conditionTag Condition.AuthorizationRevoke {} = "user.authorization.revoke"
conditionTag Condition.UserUpdate {} = "user.update"

-- EventSub transport method.
data Transport = Transport
  { transportMethod :: !Text,
    transportCallback :: !Text,
    transportSecret :: !(Maybe Text)
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("transport_" :: String)) . camelTo2 '_'} ''Transport)
