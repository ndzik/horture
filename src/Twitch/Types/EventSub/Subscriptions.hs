{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Twitch.Types.EventSub.Subscriptions
  ( SubscriptionType (..),
    IsSubscriptionType (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

data SubscriptionType
  = ChannelUpdate
  | ChannelFollow
  | ChannelSubscribe
  | ChannelSubscriptionEnd
  | ChannelSubscriptionGift
  | ChannelCheer
  | ChannelRaid
  | ChannelBan
  | ChannelUnban
  | ChannelModeratorAdd
  | ChannelModeratorRemove
  | ChannelPointsCustomRewardAdd
  | ChannelPointsCustomRewardUpdate
  | ChannelPointsCustomRewardRemove
  | ChannelPointsCustomRewardRedemptionAdd
  | ChannelPointsCustomRewardRedemptionUpdate
  | ChannelPollBegin
  | ChannelPollProgress
  | ChannelPollEnd
  | ChannelPredictionBegin
  | ChannelPredictionProgress
  | ChannelPredictionLock
  | ChannelPredictionEnd
  | CharityDonation
  | DropEntitlementGrant
  | ExtensionBitsTransactionCreate
  | GoalBegin
  | GoalProgress
  | GoalEnd
  | HypeTrainBegin
  | HypeTrainProgress
  | HypeTrainEnd
  | StreamOnline
  | StreamOffline
  | UserAuthorizationGrant
  | UserAuthorizationRevoke
  | UserUpdate
  deriving (Eq, Show)

class IsSubscriptionType (st :: SubscriptionType) where
  showST :: Text

instance IsSubscriptionType ChannelUpdate where
  showST = "channel.update"

instance IsSubscriptionType ChannelFollow where
  showST = "channel.follow"

instance IsSubscriptionType ChannelSubscribe where
  showST = "channel.subscribe"

instance IsSubscriptionType ChannelSubscriptionEnd where
  showST = "channel.subscription.end"

instance IsSubscriptionType ChannelSubscriptionGift where
  showST = "Channel.subscription.gift"

instance IsSubscriptionType ChannelCheer where
  showST = "channel.cheer"

instance IsSubscriptionType ChannelRaid where
  showST = "channel.raid"

instance IsSubscriptionType ChannelBan where
  showST = "channel.ban"

instance IsSubscriptionType ChannelUnban where
  showST = "channel.unban"

instance IsSubscriptionType ChannelModeratorAdd where
  showST = "channel.moderator.add"

instance IsSubscriptionType ChannelModeratorRemove where
  showST = "channel.moderator.remove"

instance IsSubscriptionType ChannelPointsCustomRewardAdd where
  showST = "channel.channel_points_custom_reward.add"

instance IsSubscriptionType ChannelPointsCustomRewardUpdate where
  showST = "channel.channel_points_custom_reward.upate"

instance IsSubscriptionType ChannelPointsCustomRewardRemove where
  showST = "channel.channel_points_custom_reward.remove"

instance IsSubscriptionType ChannelPointsCustomRewardRedemptionAdd where
  showST = "channel.channel_points_custom_reward_redemption.add"

instance IsSubscriptionType ChannelPointsCustomRewardRedemptionUpdate where
  showST = "channel.channel_points_custom_reward_redemption.update"

instance IsSubscriptionType ChannelPollBegin where
  showST = "channel.poll.begin"

instance IsSubscriptionType ChannelPollProgress where
  showST = "channel.poll.progress"

instance IsSubscriptionType ChannelPollEnd where
  showST = "channel.poll.end"

instance IsSubscriptionType ChannelPredictionBegin where
  showST = "channel.prediction.begin"

instance IsSubscriptionType ChannelPredictionProgress where
  showST = "channel.prediction.progress"

instance IsSubscriptionType ChannelPredictionLock where
  showST = "channel.prediction.lock"

instance IsSubscriptionType ChannelPredictionEnd where
  showST = "channel.prediction.end"

instance IsSubscriptionType CharityDonation where
  showST = "channel.charity_campaign.donate"

instance IsSubscriptionType DropEntitlementGrant where
  showST = "drop.entitlement.grant"

instance IsSubscriptionType ExtensionBitsTransactionCreate where
  showST = "extension.bits_transaction.create"

instance IsSubscriptionType GoalBegin where
  showST = "channel.goal.begin"

instance IsSubscriptionType GoalProgress where
  showST = "channel.goal.progress"

instance IsSubscriptionType GoalEnd where
  showST = "channel.goal.end"

instance IsSubscriptionType HypeTrainBegin where
  showST = "channel.hype_train.begin"

instance IsSubscriptionType HypeTrainProgress where
  showST = "channel.hype_train.progress"

instance IsSubscriptionType HypeTrainEnd where
  showST = "channel.hype_train.end"

instance IsSubscriptionType StreamOnline where
  showST = "stream.online"

instance IsSubscriptionType StreamOffline where
  showST = "stream.offline"

instance IsSubscriptionType UserAuthorizationGrant where
  showST = "user.authorization.grant"

instance IsSubscriptionType UserAuthorizationRevoke where
  showST = "user.authorization.revoke"

instance IsSubscriptionType UserUpdate where
  showST = "user.update"

$( deriveJSON
     defaultOptions
       { constructorTagModifier = \case
           "ChannelUpdate" -> "channel.update"
           "ChannelFollow" -> "channel.follow"
           "ChannelSubscribe" -> "channel.subscribe"
           "ChannelSubscriptionEnd" -> "channel.subscription.end"
           "ChannelSubscriptionGift" -> "Channel.subscription.gift"
           "ChannelCheer" -> "channel.cheer"
           "ChannelRaid" -> "channel.raid"
           "ChannelBan" -> "channel.ban"
           "ChannelUnban" -> "channel.unban"
           "ChannelModeratorAdd" -> "channel.moderator.add"
           "ChannelModeratorRemove" -> "channel.moderator.remove"
           "ChannelPointsCustomRewardAdd" -> "channel.channel_points_custom_reward.add"
           "ChannelPointsCustomRewardUpdate" -> "channel.channel_points_custom_reward.upate"
           "ChannelPointsCustomRewardRemove" -> "channel.channel_points_custom_reward.remove"
           "ChannelPointsCustomRewardRedemptionAdd" -> "channel.channel_points_custom_reward_redemption.add"
           "ChannelPointsCustomRewardRedemptionUpdate" -> "channel.channel_points_custom_reward_redemption.update"
           "ChannelPollBegin" -> "channel.poll.begin"
           "ChannelPollProgress" -> "channel.poll.progress"
           "ChannelPollEnd" -> "channel.poll.end"
           "ChannelPredictionBegin" -> "channel.prediction.begin"
           "ChannelPredictionProgress" -> "channel.prediction.progress"
           "ChannelPredictionLock" -> "channel.prediction.lock"
           "ChannelPredictionEnd" -> "channel.prediction.end"
           "CharityDonation" -> "channel.charity_campaign.donate"
           "DropEntitlementGrant" -> "drop.entitlement.grant"
           "ExtensionBitsTransactionCreate" -> "extension.bits_transaction.create"
           "GoalBegin" -> "channel.goal.begin"
           "GoalProgress" -> "channel.goal.progress"
           "GoalEnd" -> "channel.goal.end"
           "HypeTrainBegin" -> "channel.hype_train.begin"
           "HypeTrainProgress" -> "channel.hype_train.progress"
           "HypeTrainEnd" -> "channel.hype_train.end"
           "StreamOnline" -> "stream.online"
           "StreamOffline" -> "stream.offline"
           "UserAuthorizationGrant" -> "user.authorization.grant"
           "UserAuthorizationRevoke" -> "user.authorization.revoke"
           "UserUpdate" -> "user.update"
           s -> s
       }
     ''SubscriptionType
 )
