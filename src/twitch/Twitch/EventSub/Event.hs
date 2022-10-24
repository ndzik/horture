{-# LANGUAGE LambdaCase #-}

module Twitch.EventSub.Event
  ( Event (..),
    Reward (..),
    EventStatus (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.List (isPrefixOf)
import Data.Text (Text)
import Twitch.EventSub.BitsVoting
import Twitch.EventSub.ChannelPointsVoting
import Twitch.EventSub.Choices
import Twitch.EventSub.Condition
import Twitch.EventSub.Contribution
import Twitch.EventSub.EntitlementObject
import Twitch.EventSub.GlobalCooldown
import Twitch.EventSub.Image
import Twitch.EventSub.Message
import Twitch.EventSub.Outcomes
import Twitch.EventSub.Poll
import Twitch.EventSub.Product
import Twitch.EventSub.Response
import Twitch.EventSub.Reward

data Event
  = ChannelBan
      { eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventModeratorUserId :: !Text,
        eventModeratorUserLogin :: !Text,
        eventModeratorUserName :: !Text,
        eventReason :: !Text,
        eventBannedAt :: !Text,
        eventEndsAt :: !Text,
        eventIsPermanent :: !Text
      }
  | ChannelUnban
      { eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventModeratorUserId :: !Text,
        eventModeratorUserLogin :: !Text,
        eventModeratorUserName :: !Text
      }
  | ChannelSubscribe
      { eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTier :: !Text,
        eventIsGift :: !Bool
      }
  | ChannelSubscriptionEnd
      { eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTier :: !Text,
        eventIsGift :: !Bool
      }
  | ChannelSubscriptionGift
      { eventgiftUserId :: !(Maybe Text),
        eventgiftUserLogin :: !(Maybe Text),
        eventgiftUserName :: !(Maybe Text),
        eventgiftBroadcasterUserId :: !Text,
        eventgiftBroadcasterUserLogin :: !Text,
        eventgiftBroadcasterUserName :: !Text,
        eventgiftTotal :: !Int,
        eventgiftTier :: !Text,
        eventgiftCumulativeTotal :: !(Maybe Int),
        eventgiftIsAnonymous :: !Bool
      }
  | ChannelSubscriptionMessage
      { eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTier :: !Text,
        eventMessage :: !Message,
        eventCumulativeMonths :: !Int,
        eventStreakMonths :: !Int,
        eventDurationMonths :: !Int
      }
  | CharityDonation
      { eventCampaignId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventAmount :: !Amount
      }
  | ChannelCheer
      { eventIsAnonymous :: !Text,
        eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventMessage :: !Message,
        eventBits :: !Bool
      }
  | ChannelUpdate
      { eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTitle :: !Text,
        eventLanguage :: !Text,
        eventCategoryId :: !Text,
        eventCategoryName :: !Text,
        eventIsMature :: !Bool
      }
  | ChannelFollow
      { eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventFollowedAt :: !Text
      }
  | ChannelRaid
      { eventFromBroadcasterUserId :: !Text,
        eventFromBroadcasterUserLogin :: !Text,
        eventFromBroadcasterUserName :: !Text,
        eventToBroadcasterUserId :: !Text,
        eventToBroadcasterUserLogin :: !Text,
        eventToBroadcasterUserName :: !Text,
        eventViewer :: !Int
      }
  | ChannelModeratorAdd
      { eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text
      }
  | ChannelModeratorRemove
      { eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text
      }
  | ChannelPollBegin
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTitle :: !Text,
        eventChoices :: ![Choice],
        eventBitsVoting :: !(Maybe [BitsVote]),
        eventChannelPointsVoting :: !ChannelPointsVoting,
        eventStartedAt :: !Text,
        eventEndsAt :: !Text
      }
  | ChannelPollProgress
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTitle :: !Text,
        eventChoices :: ![Choice],
        eventBitsVoting :: !(Maybe [BitsVote]),
        eventChannelPointsVoting :: !ChannelPointsVoting,
        eventStartedAt :: !Text,
        eventEndsAt :: !Text
      }
  | ChannelPollEnd
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTitle :: !Text,
        eventChoices :: ![Choice],
        eventBitsVoting :: !(Maybe [BitsVote]),
        eventChannelPointsVoting :: !ChannelPointsVoting,
        eventpollStatus :: !PollStatus,
        eventStartedAt :: !Text,
        eventEndsAt :: !Text
      }
  | ChannelPointsCustomRewardAdd
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventIsEnabled :: !Bool,
        eventIsPaused :: !Bool,
        eventIsInStock :: !Bool,
        eventTitle :: !Text,
        eventCost :: !Int,
        eventPrompt :: !Text,
        eventIsUserInputRequired :: !Bool,
        eventShouldRedemptionsSkipRequestQueue :: !Bool,
        eventMaxPerStream :: !MaxPerStream,
        eventMaxPerUserPerStream :: !MaxPerUserPerStream,
        eventBackgroundColor :: !Text,
        eventImage :: !Image,
        eventDefaultImage :: !Image,
        eventGlobalCooldown :: !GlobalCooldown,
        eventCooldownExpiresAt :: !Text,
        eventRedemptionsRedeemedCurrentStream :: !Text,
        eventReward :: !Reward,
        eventRedeemedAt :: !Text
      }
  | ChannelPointsCustomRewardUpdate
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventIsEnabled :: !Bool,
        eventIsPaused :: !Bool,
        eventIsInStock :: !Bool,
        eventTitle :: !Text,
        eventCost :: !Int,
        eventPrompt :: !Text,
        eventIsUserInputRequired :: !Bool,
        eventShouldRedemptionsSkipRequestQueue :: !Bool,
        eventMaxPerStream :: !MaxPerStream,
        eventMaxPerUserPerStream :: !MaxPerUserPerStream,
        eventBackgroundColor :: !Text,
        eventImage :: !Image,
        eventDefaultImage :: !Image,
        eventGlobalCooldown :: !GlobalCooldown,
        eventCooldownExpiresAt :: !Text,
        eventRedemptionsRedeemedCurrentStream :: !Text
      }
  | ChannelPointsCustomRewardRemove
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventIsEnabled :: !Bool,
        eventIsPaused :: !Bool,
        eventIsInStock :: !Bool,
        eventTitle :: !Text,
        eventCost :: !Int,
        eventPrompt :: !Text,
        eventIsUserInputRequired :: !Bool,
        eventShouldRedemptionsSkipRequestQueue :: !Bool,
        eventMaxPerStream :: !MaxPerStream,
        eventMaxPerUserPerStream :: !MaxPerUserPerStream,
        eventBackgroundColor :: !Text,
        eventImage :: !Image,
        eventDefaultImage :: !Image,
        eventGlobalCooldown :: !GlobalCooldown,
        eventCooldownExpiresAt :: !Text,
        eventRedemptionsRedeemedCurrentStream :: !Text
      }
  | ChannelPointsCustomRewardRedemptionAdd
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventUserInput :: !Text,
        eventStatus :: !Text,
        eventReward :: !Reward,
        eventRedeemedAt :: !Text
      }
  | ChannelPointsCustomRewardRedemptionUpdate
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventUserInput :: !Text,
        eventStatus :: !Text,
        eventReward :: !Reward,
        eventRedeemedAt :: !Text
      }
  | ChannelPredictionBegin
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTitle :: !Text,
        eventOutcomes :: ![Outcome],
        eventStartedAt :: !Text,
        eventLocksAt :: !Text
      }
  | ChannelPredictionProgress
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTitle :: !Text,
        eventOutcomes :: ![Outcome],
        eventStartedAt :: !Text,
        eventLocksAt :: !Text
      }
  | ChannelPredictionLock
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTitle :: !Text,
        eventOutcomes :: ![Outcome],
        eventStartedAt :: !Text,
        eventLocksAt :: !Text
      }
  | ChannelPredictionEnd
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTitle :: !Text,
        eventOutcomes :: ![Outcome],
        eventStartedAt :: !Text,
        eventLocksAt :: !Text
      }
  | DropEntitlementGrant
      { eventId :: !Text,
        eventData :: ![EntitlementObject]
      }
  | ExtensionBitsTransactionCreate
      { eventExtensionClientId :: !Text,
        eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventUserInput :: !Text,
        eventProduct :: !Product
      }
  | Goals
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventType :: !GoalsType,
        eventDescription :: !Text,
        eventIsAchieved :: !Bool,
        eventCurrentAmount :: !Int,
        eventTargetAmount :: !Int,
        eventStartedAt :: !Text,
        eventEndedAt :: !Text
      }
  | HypeTrainBegin
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventTotal :: !Int,
        eventProgress :: !Int,
        eventGoal :: !Int,
        eventTopContributions :: ![TopContribution],
        eventLastContribution :: !LastContribution,
        eventLevel :: !Int,
        eventStartedAt :: !Text,
        eventExpiresAt :: !Text
      }
  | HypeTrainProgress
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventLevel :: !Int,
        eventTotal :: !Int,
        eventProgress :: !Int,
        eventGoal :: !Int,
        eventTopContributions :: ![TopContribution],
        eventLastContribution :: !LastContribution,
        eventStartedAt :: !Text,
        eventExpiresAt :: !Text
      }
  | HypeTrainEnd
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventLevel :: !Int,
        eventTotal :: !Int,
        eventTopContributions :: ![TopContribution],
        eventStartedAt :: !Text,
        eventEndedAt :: !Text,
        eventCooldownEndsAt :: !Text
      }
  | StreamOnline
      { eventId :: !Text,
        eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text,
        eventstreamType :: !StreamType,
        eventStartedAt :: !Text
      }
  | StreamOffline
      { eventBroadcasterUserId :: !Text,
        eventBroadcasterUserLogin :: !Text,
        eventBroadcasterUserName :: !Text
      }
  | AuthorizationGrant
      { eventClientId :: !Text,
        eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text
      }
  | AuthorizationRevoke
      { eventClientId :: !Text,
        eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text
      }
  | UserUpdate
      { eventUserId :: !Text,
        eventUserLogin :: !Text,
        eventUserName :: !Text,
        eventEmail :: !Text,
        eventEmailVerified :: !Bool,
        eventDescription :: !Text
      }
  deriving (Show)

data StreamType
  = Live
  | Playlist
  | WatchParty
  | Premiere
  | Rerun
  deriving (Show)

data GoalsType
  = Follow
  | Subscription
  | SubscriptionCount
  | NewSubscription
  | NewSubscriptionCount
  deriving (Show)

data EventStatus
  = Unfulfilled
  | Unknown
  | Fulfilled
  | Canceled
  deriving (Show)

data Amount = Amount
  { amountValue :: !Int,
    amountDecimalPlaces :: !Int,
    amountCurrency :: !Text
  }
  deriving (Show)

data Reward = Reward
  { rrewardId :: !Text,
    rrewardTitle :: !Text,
    rrewardCost :: !Int,
    rrewardPrompt :: !Text
  }
  deriving (Show)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o ->
    (.:) @Object o "event" >>= \evo ->
    (.:) @ResponseObject o "subscription" >>= (\case
      ChannelUpdateCondition {} -> ChannelUpdate <$> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "title" <*> evo .: "language" <*> evo .: "category_id" <*> evo .: "category_name" <*> evo .: "is_mature"
      ChannelFollowCondition {} -> ChannelFollow <$> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "followed_at"
      ChannelSubscribeCondition {} -> ChannelSubscribe <$> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "tier" <*> evo .: "is_gift"
      ChannelSubscriptionEndCondition {} -> ChannelSubscriptionEnd <$> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "tier" <*> evo .: "is_gift"
      ChannelSubscriptionGiftCondition {} -> ChannelSubscriptionGift <$> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "total" <*> evo .: "tier" <*> evo .: "cumulative_total" <*> evo .: "is_anonymous"
      ChannelCheerCondition {} -> ChannelCheer <$> evo .: "is_anonymous" <*> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "message" <*> evo .: "bits"
      ChannelRaidCondition {} -> ChannelRaid <$> evo .: "from_broadcaster_user_id" <*> evo .: "from_broadcaster_user_login" <*> evo .: "from_broadcaster_user_name" <*> evo .: "to_broadcaster_user_id" <*> evo .: "to_broadcaster_user_login" <*> evo .: "to_broadcaster_user_name" <*> evo .: "viewer"
      ChannelBanCondition {} -> ChannelBan <$> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "moderator_user_id" <*> evo .: "moderator_user_login" <*> evo .: "moderator_user_name" <*> evo .: "reason" <*> evo .: "banned_at" <*> evo .: "ends_at" <*> evo .: "is_permanent"
      ChannelUnbanCondition {} -> ChannelUnban <$> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "moderator_user_id" <*> evo .: "moderator_user_login" <*> evo .: "moderator_user_name"
      ChannelModeratorAddCondition {} -> ChannelModeratorAdd <$> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name"
      ChannelModeratorRemoveCondition {} -> ChannelModeratorRemove <$> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name"
      ChannelPointsCustomRewardAddCondition {} -> ChannelPointsCustomRewardAdd <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "is_enabled" <*> evo .: "is_paused" <*> evo .: "is_in_stock" <*> evo .: "title" <*> evo .: "cost" <*> evo .: "prompt" <*> evo .: "is_user_input_required" <*> evo .: "should_redemptions_skip_request_queue" <*> evo .: "max_per_stream" <*> evo .: "max_per_user_per_stream" <*> evo .: "background_color" <*> evo .: "image" <*> evo .: "default_image" <*> evo .: "global_cooldown" <*> evo .: "cooldown_expires_at" <*> evo .: "redemptions_redeemed_current_stream" <*> evo .: "reward" <*> evo .: "redeemed_at"
      ChannelPointsCustomRewardUpdateCondition {} -> ChannelPointsCustomRewardUpdate <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "is_enabled" <*> evo .: "is_paused" <*> evo .: "is_in_stock" <*> evo .: "title" <*> evo .: "cost" <*> evo .: "prompt" <*> evo .: "is_user_input_required" <*> evo .: "should_redemptions_skip_request_queue" <*> evo .: "max_per_stream" <*> evo .: "max_per_user_per_stream" <*> evo .: "background_color" <*> evo .: "image" <*> evo .: "default_image" <*> evo .: "global_cooldown" <*> evo .: "cooldown_expires_at" <*> evo .: "redemptions_redeemed_current_stream"
      ChannelPointsCustomRewardRemoveCondition {} -> ChannelPointsCustomRewardRemove <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "is_enabled" <*> evo .: "is_paused" <*> evo .: "is_in_stock" <*> evo .: "title" <*> evo .: "cost" <*> evo .: "prompt" <*> evo .: "is_user_input_required" <*> evo .: "should_redemptions_skip_request_queue" <*> evo .: "max_per_stream" <*> evo .: "max_per_user_per_stream" <*> evo .: "background_color" <*> evo .: "image" <*> evo .: "default_image" <*> evo .: "global_cooldown" <*> evo .: "cooldown_expires_at" <*> evo .: "redemptions_redeemed_current_stream"
      ChannelPointsCustomRewardRedemptionAddCondition {} -> ChannelPointsCustomRewardRedemptionAdd <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "user_input" <*> evo .: "status" <*> evo .: "reward" <*> evo .: "redeemed_at"
      ChannelPointsCustomRewardRedemptionUpdateCondition {} -> ChannelPointsCustomRewardRedemptionUpdate <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "user_input" <*> evo .: "status" <*> evo .: "reward" <*> evo .: "redeemed_at"
      ChannelPollBeginCondition {} -> ChannelPollBegin <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "title" <*> evo .: "choices" <*> evo .:? "bits_voting" <*> evo .: "channel_points_voting" <*> evo .: "started_at" <*> evo .: "ends_at"
      ChannelPollProgressCondition {} -> ChannelPollProgress <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "title" <*> evo .: "choices" <*> evo .:? "bits_voting" <*> evo .: "channel_points_voting" <*> evo .: "started_at" <*> evo .: "ends_at"
      ChannelPollEndCondition {} -> ChannelPollEnd <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "title" <*> evo .: "choices" <*> evo .:? "bits_voting" <*> evo .: "channel_points_voting" <*> evo .: "status" <*> evo .: "started_at" <*> evo .: "ends_at"
      ChannelPredictionBeginCondition {} -> ChannelPredictionBegin <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "title" <*> evo .: "outcomes" <*> evo .: "started_at" <*> evo .: "locks_at"
      ChannelPredictionProgressCondition {} -> ChannelPredictionProgress <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "title" <*> evo .: "outcomes" <*> evo .: "started_at" <*> evo .: "locks_at"
      ChannelPredictionLockCondition {} -> ChannelPredictionLock <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "title" <*> evo .: "outcomes" <*> evo .: "started_at" <*> evo .: "locks_at"
      ChannelPredictionEndCondition {} -> ChannelPredictionEnd <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "title" <*> evo .: "outcomes" <*> evo .: "started_at" <*> evo .: "locks_at"
      DropEntitlementGrantCondition {} -> DropEntitlementGrant <$> evo .: "id" <*> evo .: "data"
      ExtensionBitsTransactionCreateCondition {} -> ExtensionBitsTransactionCreate <$> evo .: "extension_client_id" <*> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "user_input" <*> evo .: "product"
      GoalBeginCondition {} -> Goals <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "type" <*> evo .: "description" <*> evo .: "is_achieved" <*> evo .: "current_amount" <*> evo .: "target_amount" <*> evo .: "started_at" <*> evo .: "ended_at"
      GoalProgressCondition {} -> Goals <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "type" <*> evo .: "description" <*> evo .: "is_achieved" <*> evo .: "current_amount" <*> evo .: "target_amount" <*> evo .: "started_at" <*> evo .: "ended_at"
      GoalEndCondition {} -> Goals <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "type" <*> evo .: "description" <*> evo .: "is_achieved" <*> evo .: "current_amount" <*> evo .: "target_amount" <*> evo .: "started_at" <*> evo .: "ended_at"
      HypeTrainBeginCondition {} -> HypeTrainBegin <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "total" <*> evo .: "progress" <*> evo .: "goal" <*> evo .: "top_contributions" <*> evo .: "last_contribution" <*> evo .: "level" <*> evo .: "started_at" <*> evo .: "expires_at"
      HypeTrainProgressCondition {} -> HypeTrainProgress <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "level" <*> evo .: "total" <*> evo .: "progress" <*> evo .: "goal" <*> evo .: "top_contributions" <*> evo .: "last_contribution" <*> evo .: "started_at" <*> evo .: "expires_at"
      HypeTrainEndCondition {} -> HypeTrainEnd <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "level" <*> evo .: "total" <*> evo .: "top_contributions" <*> evo .: "started_at" <*> evo .: "ended_at" <*> evo .: "cooldown_ends_at"
      StreamOnlineCondition {} -> StreamOnline <$> evo .: "id" <*> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name" <*> evo .: "type" <*> evo .: "started_at"
      StreamOfflineCondition {} -> StreamOffline <$> evo .: "broadcaster_user_id" <*> evo .: "broadcaster_user_login" <*> evo .: "broadcaster_user_name"
      AuthorizationGrantCondition {} -> AuthorizationGrant <$> evo .: "client_id" <*> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name"
      AuthorizationRevokeCondition {} -> AuthorizationRevoke <$> evo .: "client_id" <*> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name"
      UserUpdateCondition {} -> UserUpdate <$> evo .: "user_id" <*> evo .: "user_login" <*> evo .: "user_name" <*> evo .: "email" <*> evo .: "email_verified" <*> evo .: "description"
                                                                          ) . responseobjectCondition

$(deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''StreamType)
$(deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''EventStatus)
$(deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''GoalsType)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("rreward_" :: String)) . camelTo2 '_'} ''Reward)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("amount_" :: String)) . camelTo2 '_'} ''Amount)
$( deriveToJSON
     defaultOptions
       { fieldLabelModifier =
           \case
             s | isPrefixOf "eventgift" s -> drop (length ("eventgift_" :: String)) . camelTo2 '_' $ s
             s | isPrefixOf "eventstream" s -> drop (length ("eventstream_" :: String)) . camelTo2 '_' $ s
             s -> drop (length ("event_" :: String)) . camelTo2 '_' $ s
       }
     ''Event
 )
