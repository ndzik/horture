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
  = ChannelPointsCustomRewardAddCondition
      { broadcasterUserId :: !Text
      }
  | ChannelPointsCustomRewardUpdateCondition
      { broadcasterUserId :: !Text,
        rewardId :: !(Maybe Text)
      }
  | ChannelPointsCustomRewardRemoveCondition
      { broadcasterUserId :: !Text,
        rewardId :: !(Maybe Text)
      }
  | ChannelPointsCustomRewardRedemptionAddCondition
      { broadcasterUserId :: !Text,
        rewardId :: !(Maybe Text)
      }
  | ChannelPointsCustomRewardRedemptionUpdateCondition
      { broadcasterUserId :: !Text,
        rewardId :: !(Maybe Text)
      }
  | ChannelPollBeginCondition
      { broadcasterUserId :: !Text
      }
  | ChannelPollProgressCondition
      { broadcasterUserId :: !Text
      }
  | ChannelPollEndCondition
      { broadcasterUserId :: !Text
      }
  | ChannelPredictionBeginCondition
      { userId :: !Text
      }
  | ChannelPredictionProgressCondition
      { userId :: !Text
      }
  | ChannelPredictionLockCondition
      { userId :: !Text
      }
  | ChannelPredictionEndCondition
      { userId :: !Text
      }
  | ChannelRaidCondition
      { fromBroadcasterUserId :: !(Maybe Text),
        toBroadcasterUserId :: !(Maybe Text)
      }
  | DropEntitlementGrantCondition
      { organizationId :: !Text,
        categoryId :: !(Maybe Text),
        campaignId :: !(Maybe Text)
      }
  | ChannelFollowCondition
      { broadcasterUserId :: !Text
      }
  | ChannelUpdateCondition
      { broadcasterUserId :: !Text
      }
  | ChannelCheerCondition
      { broadcasterUserId :: !Text
      }
  | ChannelSubscribeCondition
      { broadcasterUserId :: !Text
      }
  | ChannelSubscriptionEndCondition
      { broadcasterUserId :: !Text
      }
  | ChannelSubscriptionGiftCondition
      { broadcasterUserId :: !Text
      }
  | ChannelBanCondition
      { broadcasterUserId :: !Text
      }
  | ChannelUnbanCondition
      { broadcasterUserId :: !Text
      }
  | ChannelModeratorAddCondition
      { broadcasterUserId :: !Text
      }
  | ChannelModeratorRemoveCondition
      { broadcasterUserId :: !Text
      }
  | GoalBeginCondition
      { broadcasterUserId :: !Text
      }
  | GoalProgressCondition
      { broadcasterUserId :: !Text
      }
  | GoalEndCondition
      { broadcasterUserId :: !Text
      }
  | HypeTrainBeginCondition
      { broadcasterUserId :: !Text
      }
  | HypeTrainProgressCondition
      { broadcasterUserId :: !Text
      }
  | HypeTrainEndCondition
      { broadcasterUserId :: !Text
      }
  | StreamOnlineCondition
      { broadcasterUserId :: !Text
      }
  | StreamOfflineCondition
      { broadcasterUserId :: !Text
      }
  | AuthorizationGrantCondition
      { clientId :: !Text
      }
  | AuthorizationRevokeCondition
      { clientId :: !Text
      }
  | UserUpdateCondition
      { userId :: !Text
      }
  | ExtensionBitsTransactionCreateCondition
      { clientId :: !Text
      }
  deriving (Show)

instance FromJSON Condition where
  parseJSON = withObject "Condition" $ \o ->
    (.:) @Text o "type" >>= \case
      "channel.update" -> do
        condition <- o .: "condition"
        ChannelUpdateCondition <$> condition .: "broadcaster_user_id"
      "channel.follow" -> do
        condition <- o .: "condition"
        ChannelFollowCondition <$> condition .: "broadcaster_user_id"
      "channel.subscribe" -> do
        condition <- o .: "condition"
        ChannelSubscribeCondition <$> condition .: "broadcaster_user_id"
      "channel.subscription.end" -> do
        condition <- o .: "condition"
        ChannelSubscriptionEndCondition <$> condition .: "broadcaster_user_id"
      "Channel.subscription.gift" -> do
        condition <- o .: "condition"
        ChannelSubscriptionGiftCondition <$> condition .: "broadcaster_user_id"
      "channel.cheer" -> do
        condition <- o .: "condition"
        ChannelCheerCondition <$> condition .: "broadcaster_user_id"
      "channel.raid" -> do
        condition <- o .: "condition"
        ChannelRaidCondition <$> condition .:? "from_broadcaster_user_id" <*> condition .:? "to_broadcaster_user_id"
      "channel.ban" -> do
        condition <- o .: "condition"
        ChannelBanCondition <$> condition .: "broadcaster_user_id"
      "channel.unban" -> do
        condition <- o .: "condition"
        ChannelUnbanCondition <$> condition .: "broadcaster_user_id"
      "channel.moderator.add" -> do
        condition <- o .: "condition"
        ChannelModeratorAddCondition <$> condition .: "broadcaster_user_id"
      "channel.moderator.remove" -> do
        condition <- o .: "condition"
        ChannelModeratorRemoveCondition <$> condition .: "broadcaster_user_id"
      "channel.channel_points_custom_reward.add" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardAddCondition <$> condition .: "broadcaster_user_id"
      "channel.channel_points_custom_reward.update" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardUpdateCondition <$> condition .: "broadcaster_user_id" <*> condition .:? "reward_id"
      "channel.channel_points_custom_reward.remove" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardRemoveCondition <$> condition .: "broadcaster_user_id" <*> condition .:? "reward_id"
      "channel.channel_points_custom_reward_redemption.add" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardRedemptionAddCondition <$> condition .: "broadcaster_user_id" <*> condition .:? "reward_id"
      "channel.channel_points_custom_reward_redemption.update" -> do
        condition <- o .: "condition"
        ChannelPointsCustomRewardRedemptionUpdateCondition <$> condition .: "broadcaster_user_id" <*> condition .:? "reward_id"
      "channel.poll.begin" -> do
        condition <- o .: "condition"
        ChannelPollBeginCondition <$> condition .: "broadcaster_user_id"
      "channel.poll.progress" -> do
        condition <- o .: "condition"
        ChannelPollProgressCondition <$> condition .: "broadcaster_user_id"
      "channel.poll.end" -> do
        condition <- o .: "condition"
        ChannelPollEndCondition <$> condition .: "broadcaster_user_id"
      "channel.prediction.begin" -> do
        condition <- o .: "condition"
        ChannelPredictionBeginCondition <$> condition .: "user_id"
      "channel.prediction.progress" -> do
        condition <- o .: "condition"
        ChannelPredictionProgressCondition <$> condition .: "user_id"
      "channel.prediction.lock" -> do
        condition <- o .: "condition"
        ChannelPredictionLockCondition <$> condition .: "user_id"
      "channel.prediction.end" -> do
        condition <- o .: "condition"
        ChannelPredictionEndCondition <$> condition .: "user_id"
      "drop.entitlement.grant" -> do
        condition <- o .: "condition"
        DropEntitlementGrantCondition <$> condition .: "organization_id" <*> condition .:? "category_id" <*> condition .:? "campaign_id"
      "extension.bits_transaction.create" -> do
        condition <- o .: "condition"
        ExtensionBitsTransactionCreateCondition <$> condition .: "client_id"
      "channel.goal.begin" -> do
        condition <- o .: "condition"
        GoalBeginCondition <$> condition .: "broadcaster_user_id"
      "channel.goal.progress" -> do
        condition <- o .: "condition"
        GoalProgressCondition <$> condition .: "broadcaster_user_id"
      "channel.goal.end" -> do
        condition <- o .: "condition"
        GoalEndCondition <$> condition .: "broadcaster_user_id"
      "channel.hype_train.begin" -> do
        condition <- o .: "condition"
        HypeTrainBeginCondition <$> condition .: "broadcaster_user_id"
      "channel.hype_train.progress" -> do
        condition <- o .: "condition"
        HypeTrainProgressCondition <$> condition .: "broadcaster_user_id"
      "channel.hype_train.end" -> do
        condition <- o .: "condition"
        HypeTrainEndCondition <$> condition .: "broadcaster_user_id"
      "stream.online" -> do
        condition <- o .: "condition"
        StreamOnlineCondition <$> condition .: "broadcaster_user_id"
      "stream.offline" -> do
        condition <- o .: "condition"
        StreamOfflineCondition <$> condition .: "broadcaster_user_id"
      "user.authorization.grant" -> do
        condition <- o .: "condition"
        AuthorizationGrantCondition <$> condition .: "client_id"
      "user.authorization.revoke" -> do
        condition <- o .: "condition"
        AuthorizationRevokeCondition <$> condition .: "client_id"
      "user.update" -> do
        condition <- o .: "condition"
        UserUpdateCondition <$> condition .: "user_id"
      _otherwise -> parseFail "unhandled subscription type"

conditionTag :: Condition -> Text
conditionTag ChannelUpdateCondition {} = "channel.update"
conditionTag ChannelFollowCondition {} = "channel.follow"
conditionTag ChannelSubscribeCondition {} = "channel.subscribe"
conditionTag ChannelSubscriptionEndCondition {} = "channel.subscription.end"
conditionTag ChannelSubscriptionGiftCondition {} = "Channel.subscription.gift"
conditionTag ChannelCheerCondition {} = "channel.cheer"
conditionTag ChannelRaidCondition {} = "channel.raid"
conditionTag ChannelBanCondition {} = "channel.ban"
conditionTag ChannelUnbanCondition {} = "channel.unban"
conditionTag ChannelModeratorAddCondition {} = "channel.moderator.add"
conditionTag ChannelModeratorRemoveCondition {} = "channel.moderator.remove"
conditionTag ChannelPointsCustomRewardAddCondition {} = "channel.channel_points_custom_reward.add"
conditionTag ChannelPointsCustomRewardUpdateCondition {} = "channel.channel_points_custom_reward.update"
conditionTag ChannelPointsCustomRewardRemoveCondition {} = "channel.channel_points_custom_reward.remove"
conditionTag ChannelPointsCustomRewardRedemptionAddCondition {} = "channel.channel_points_custom_reward_redemption.add"
conditionTag ChannelPointsCustomRewardRedemptionUpdateCondition {} = "channel.channel_points_custom_reward_redemption.update"
conditionTag ChannelPollBeginCondition {} = "channel.poll.begin"
conditionTag ChannelPollProgressCondition {} = "channel.poll.progress"
conditionTag ChannelPollEndCondition {} = "channel.poll.end"
conditionTag ChannelPredictionBeginCondition {} = "channel.prediction.begin"
conditionTag ChannelPredictionProgressCondition {} = "channel.prediction.progress"
conditionTag ChannelPredictionLockCondition {} = "channel.prediction.lock"
conditionTag ChannelPredictionEndCondition {} = "channel.prediction.end"
conditionTag DropEntitlementGrantCondition {} = "drop.entitlement.grant"
conditionTag ExtensionBitsTransactionCreateCondition {} = "extension.bits_transaction.create"
conditionTag GoalBeginCondition {} = "channel.goal.begin"
conditionTag GoalProgressCondition {} = "channel.goal.progress"
conditionTag GoalEndCondition {} = "channel.goal.end"
conditionTag HypeTrainBeginCondition {} = "channel.hype_train.begin"
conditionTag HypeTrainProgressCondition {} = "channel.hype_train.progress"
conditionTag HypeTrainEndCondition {} = "channel.hype_train.end"
conditionTag StreamOnlineCondition {} = "stream.online"
conditionTag StreamOfflineCondition {} = "stream.offline"
conditionTag AuthorizationGrantCondition {} = "user.authorization.grant"
conditionTag AuthorizationRevokeCondition {} = "user.authorization.revoke"
conditionTag UserUpdateCondition {} = "user.update"

$(deriveToJSON defaultOptions {fieldLabelModifier = camelTo2 '_', sumEncoding = UntaggedValue} ''Condition)
