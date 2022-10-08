{-# LANGUAGE LambdaCase #-}

-- | Condition types according to the twitch specification:
--
-- https://dev.twitch.tv/docs/eventsub/eventsub-reference#conditions
module Twitch.EventSub.Condition
  ( Condition (..),
    conditionTag,
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import Prelude hiding (unwords)

data Condition
  = ChannelPointsCustomRewardAdd
      { broadcasterUserId :: !Text
      }
  | ChannelPointsCustomRewardUpdate
      { broadcasterUserId :: !Text,
        rewardId :: !(Maybe Text)
      }
  | ChannelPointsCustomRewardRemove
      { broadcasterUserId :: !Text,
        rewardId :: !(Maybe Text)
      }
  | ChannelPointsCustomRewardRedemptionAdd
      { broadcasterUserId :: !Text,
        rewardId :: !(Maybe Text)
      }
  | ChannelPointsCustomRewardRedemptionUpdate
      { broadcasterUserId :: !Text,
        rewardId :: !(Maybe Text)
      }
  | ChannelPollBegin
      { broadcasterUserId :: !Text
      }
  | ChannelPollProgress
      { broadcasterUserId :: !Text
      }
  | ChannelPollEnd
      { broadcasterUserId :: !Text
      }
  | ChannelPredictionBegin
      { userId :: !Text
      }
  | ChannelPredictionProgress
      { userId :: !Text
      }
  | ChannelPredictionLock
      { userId :: !Text
      }
  | ChannelPredictionEnd
      { userId :: !Text
      }
  | ChannelRaid
      { fromBroadcasterUserId :: !(Maybe Text),
        toBroadcasterUserId :: !(Maybe Text)
      }
  | DropEntitlementGrant
      { organizationId :: !Text,
        categoryId :: !(Maybe Text),
        campaignId :: !(Maybe Text)
      }
  | ChannelFollow
      { broadcasterUserId :: !Text
      }
  | ChannelUpdate
      { broadcasterUserId :: !Text
      }
  | ChannelCheer
      { broadcasterUserId :: !Text
      }
  | ChannelSubscribe
      { broadcasterUserId :: !Text
      }
  | ChannelSubscriptionEnd
      { broadcasterUserId :: !Text
      }
  | ChannelSubscriptionGift
      { broadcasterUserId :: !Text
      }
  | ChannelBan
      { broadcasterUserId :: !Text
      }
  | ChannelUnban
      { broadcasterUserId :: !Text
      }
  | ChannelModeratorAdd
      { broadcasterUserId :: !Text
      }
  | ChannelModeratorRemove
      { broadcasterUserId :: !Text
      }
  | GoalBegin
      { broadcasterUserId :: !Text
      }
  | GoalProgress
      { broadcasterUserId :: !Text
      }
  | GoalEnd
      { broadcasterUserId :: !Text
      }
  | HypeTrainBegin
      { broadcasterUserId :: !Text
      }
  | HypeTrainProgress
      { broadcasterUserId :: !Text
      }
  | HypeTrainEnd
      { broadcasterUserId :: !Text
      }
  | StreamOnline
      { broadcasterUserId :: !Text
      }
  | StreamOffline
      { broadcasterUserId :: !Text
      }
  | AuthorizationGrant
      { clientId :: !Text
      }
  | AuthorizationRevoke
      { clientId :: !Text
      }
  | UserUpdate
      { userId :: !Text
      }
  | ExtensionBitsTransactionCreate
      { clientId :: !Text
      }
  deriving (Show)

instance FromJSON Condition where
  parseJSON = withObject "Condition" $ \o ->
    (.:) @Text o "type" >>= \case
      "channel.update" -> do
        condition <- o .: "condition"
        ChannelUpdate <$> condition .: "broadcaster_user_id"
      "channel.follow" -> do
        condition <- o .: "condition"
        ChannelFollow <$> condition .: "broadcaster_user_id"
      "channel.subscribe" -> do
        condition <- o .: "condition"
        ChannelSubscribe <$> condition .: "broadcaster_user_id"
      "channel.subscription.end" -> do
        condition <- o .: "condition"
        ChannelSubscriptionEnd <$> condition .: "broadcaster_user_id"
      "Channel.subscription.gift" -> do
        condition <- o .: "condition"
        ChannelSubscriptionGift <$> condition .: "broadcaster_user_id"
      "channel.cheer" -> do
        condition <- o .: "condition"
        ChannelCheer <$> condition .: "broadcaster_user_id"
      "channel.raid" -> do
        condition <- o .: "condition"
        ChannelRaid <$> condition .:? "from_broadcaster_user_id" <*> condition .:? "to_broadcaster_user_id"
      "channel.ban" -> do
        condition <- o .: "condition"
        ChannelBan <$> condition .: "broadcaster_user_id"
      "channel.unban" -> do
        condition <- o .: "condition"
        ChannelUnban <$> condition .: "broadcaster_user_id"
      "channel.moderator.add" -> do
        condition <- o .: "condition"
        ChannelModeratorAdd <$> condition .: "broadcaster_user_id"
      "channel.moderator.remove" -> do
        condition <- o .: "condition"
        ChannelModeratorRemove <$> condition .: "broadcaster_user_id"
      "channel.channel_points_custom_reward.add" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardAdd <$> condition .: "broadcaster_user_id"
      "channel.channel_points_custom_reward.update" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardUpdate <$> condition .: "broadcaster_user_id" <*> condition .:? "reward_id"
      "channel.channel_points_custom_reward.remove" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardRemove <$> condition .: "broadcaster_user_id" <*> condition .:? "reward_id"
      "channel.channel_points_custom_reward_redemption.add" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardRedemptionAdd <$> condition .: "broadcaster_user_id" <*> condition .:? "reward_id"
      "channel.channel_points_custom_reward_redemption.update" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardRedemptionUpdate <$> condition .: "broadcaster_user_id" <*> condition .:? "reward_id"
      "channel.poll.begin" -> do
        condition <- o .: "condition"
        ChannelPollBegin <$> condition .: "broadcaster_user_id"
      "channel.poll.progress" -> do
        condition <- o .: "condition"
        ChannelPollProgress <$> condition .: "broadcaster_user_id"
      "channel.poll.end" -> do
        condition <- o .: "condition"
        ChannelPollEnd <$> condition .: "broadcaster_user_id"
      "channel.prediction.begin" -> do
        condition <- o .: "condition"
        ChannelPredictionBegin <$> condition .: "user_id"
      "channel.prediction.progress" -> do
        condition <- o .: "condition"
        ChannelPredictionProgress <$> condition .: "user_id"
      "channel.prediction.lock" -> do
        condition <- o .: "condition"
        ChannelPredictionLock <$> condition .: "user_id"
      "channel.prediction.end" -> do
        condition <- o .: "condition"
        ChannelPredictionEnd <$> condition .: "user_id"
      "drop.entitlement.grant" -> do
        condition <- o .: "condition"
        DropEntitlementGrant <$> condition .: "organization_id" <*> condition .:? "category_id" <*> condition .:? "campaign_id"
      "extension.bits_transaction.create" -> do
        condition <- o .: "condition"
        ExtensionBitsTransactionCreate <$> condition .: "client_id"
      "channel.goal.begin" -> do
        condition <- o .: "condition"
        GoalBegin <$> condition .: "broadcaster_user_id"
      "channel.goal.progress" -> do
        condition <- o .: "condition"
        GoalProgress <$> condition .: "broadcaster_user_id"
      "channel.goal.end" -> do
        condition <- o .: "condition"
        GoalEnd <$> condition .: "broadcaster_user_id"
      "channel.hype_train.begin" -> do
        condition <- o .: "condition"
        HypeTrainBegin <$> condition .: "broadcaster_user_id"
      "channel.hype_train.progress" -> do
        condition <- o .: "condition"
        HypeTrainProgress <$> condition .: "broadcaster_user_id"
      "channel.hype_train.end" -> do
        condition <- o .: "condition"
        HypeTrainEnd <$> condition .: "broadcaster_user_id"
      "stream.online" -> do
        condition <- o .: "condition"
        StreamOnline <$> condition .: "broadcaster_user_id"
      "stream.offline" -> do
        condition <- o .: "condition"
        StreamOffline <$> condition .: "broadcaster_user_id"
      "user.authorization.grant" -> do
        condition <- o .: "condition"
        AuthorizationGrant <$> condition .: "client_id"
      "user.authorization.revoke" -> do
        condition <- o .: "condition"
        AuthorizationRevoke <$> condition .: "client_id"
      "user.update" -> do
        condition <- o .: "condition"
        UserUpdate <$> condition .: "user_id"
      _otherwise -> parseFail "unhandled subscription type"

conditionTag :: Condition -> Text
conditionTag ChannelUpdate {} = "channel.update"
conditionTag ChannelFollow {} = "channel.follow"
conditionTag ChannelSubscribe {} = "channel.subscribe"
conditionTag ChannelSubscriptionEnd {} = "channel.subscription.end"
conditionTag ChannelSubscriptionGift {} = "Channel.subscription.gift"
conditionTag ChannelCheer {} = "channel.cheer"
conditionTag ChannelRaid {} = "channel.raid"
conditionTag ChannelBan {} = "channel.ban"
conditionTag ChannelUnban {} = "channel.unban"
conditionTag ChannelModeratorAdd {} = "channel.moderator.add"
conditionTag ChannelModeratorRemove {} = "channel.moderator.remove"
conditionTag ChannelPointsCustomRewardAdd {} = "channel.channel_points_custom_reward.add"
conditionTag ChannelPointsCustomRewardUpdate {} = "channel.channel_points_custom_reward.update"
conditionTag ChannelPointsCustomRewardRemove {} = "channel.channel_points_custom_reward.remove"
conditionTag ChannelPointsCustomRewardRedemptionAdd {} = "channel.channel_points_custom_reward_redemption.add"
conditionTag ChannelPointsCustomRewardRedemptionUpdate {} = "channel.channel_points_custom_reward_redemption.update"
conditionTag ChannelPollBegin {} = "channel.poll.begin"
conditionTag ChannelPollProgress {} = "channel.poll.progress"
conditionTag ChannelPollEnd {} = "channel.poll.end"
conditionTag ChannelPredictionBegin {} = "channel.prediction.begin"
conditionTag ChannelPredictionProgress {} = "channel.prediction.progress"
conditionTag ChannelPredictionLock {} = "channel.prediction.lock"
conditionTag ChannelPredictionEnd {} = "channel.prediction.end"
conditionTag DropEntitlementGrant {} = "drop.entitlement.grant"
conditionTag ExtensionBitsTransactionCreate {} = "extension.bits_transaction.create"
conditionTag GoalBegin {} = "channel.goal.begin"
conditionTag GoalProgress {} = "channel.goal.progress"
conditionTag GoalEnd {} = "channel.goal.end"
conditionTag HypeTrainBegin {} = "channel.hype_train.begin"
conditionTag HypeTrainProgress {} = "channel.hype_train.progress"
conditionTag HypeTrainEnd {} = "channel.hype_train.end"
conditionTag StreamOnline {} = "stream.online"
conditionTag StreamOffline {} = "stream.offline"
conditionTag AuthorizationGrant {} = "user.authorization.grant"
conditionTag AuthorizationRevoke {} = "user.authorization.revoke"
conditionTag UserUpdate {} = "user.update"

$(deriveToJSON defaultOptions {fieldLabelModifier = camelTo2 '_', sumEncoding = UntaggedValue} ''Condition)
