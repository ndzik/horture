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
     (.:) @ResponseObject o "subscription" >>= (\case
      ChannelUpdateCondition {} -> ChannelUpdate <$> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "title" <*> o .: "language" <*> o .: "category_id" <*> o .: "category_name" <*> o .: "is_mature"
      ChannelFollowCondition {} -> ChannelFollow <$> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "followed_at"
      ChannelSubscribeCondition {} -> ChannelSubscribe <$> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "tier" <*> o .: "is_gift"
      ChannelSubscriptionEndCondition {} -> ChannelSubscriptionEnd <$> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "tier" <*> o .: "is_gift"
      ChannelSubscriptionGiftCondition {} -> ChannelSubscriptionGift <$> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "total" <*> o .: "tier" <*> o .: "cumulative_total" <*> o .: "is_anonymous"
      ChannelCheerCondition {} -> ChannelCheer <$> o .: "is_anonymous" <*> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "message" <*> o .: "bits"
      ChannelRaidCondition {} -> ChannelRaid <$> o .: "from_broadcaster_user_id" <*> o .: "from_broadcaster_user_login" <*> o .: "from_broadcaster_user_name" <*> o .: "to_broadcaster_user_id" <*> o .: "to_broadcaster_user_login" <*> o .: "to_broadcaster_user_name" <*> o .: "viewer"
      ChannelBanCondition {} -> ChannelBan <$> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "moderator_user_id" <*> o .: "moderator_user_login" <*> o .: "moderator_user_name" <*> o .: "reason" <*> o .: "banned_at" <*> o .: "ends_at" <*> o .: "is_permanent"
      ChannelUnbanCondition {} -> ChannelUnban <$> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "moderator_user_id" <*> o .: "moderator_user_login" <*> o .: "moderator_user_name"
      ChannelModeratorAddCondition {} -> ChannelModeratorAdd <$> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name"
      ChannelModeratorRemoveCondition {} -> ChannelModeratorRemove <$> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name"
      ChannelPointsCustomRewardAddCondition {} -> ChannelPointsCustomRewardAdd <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "is_enabled" <*> o .: "is_paused" <*> o .: "is_in_stock" <*> o .: "title" <*> o .: "cost" <*> o .: "prompt" <*> o .: "is_user_input_required" <*> o .: "should_redemptions_skip_request_queue" <*> o .: "max_per_stream" <*> o .: "max_per_user_per_stream" <*> o .: "background_color" <*> o .: "image" <*> o .: "default_image" <*> o .: "global_cooldown" <*> o .: "cooldown_expires_at" <*> o .: "redemptions_redeemed_current_stream" <*> o .: "reward" <*> o .: "redeemed_at"
      ChannelPointsCustomRewardUpdateCondition {} -> ChannelPointsCustomRewardUpdate <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "is_enabled" <*> o .: "is_paused" <*> o .: "is_in_stock" <*> o .: "title" <*> o .: "cost" <*> o .: "prompt" <*> o .: "is_user_input_required" <*> o .: "should_redemptions_skip_request_queue" <*> o .: "max_per_stream" <*> o .: "max_per_user_per_stream" <*> o .: "background_color" <*> o .: "image" <*> o .: "default_image" <*> o .: "global_cooldown" <*> o .: "cooldown_expires_at" <*> o .: "redemptions_redeemed_current_stream"
      ChannelPointsCustomRewardRemoveCondition {} -> ChannelPointsCustomRewardRemove <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "is_enabled" <*> o .: "is_paused" <*> o .: "is_in_stock" <*> o .: "title" <*> o .: "cost" <*> o .: "prompt" <*> o .: "is_user_input_required" <*> o .: "should_redemptions_skip_request_queue" <*> o .: "max_per_stream" <*> o .: "max_per_user_per_stream" <*> o .: "background_color" <*> o .: "image" <*> o .: "default_image" <*> o .: "global_cooldown" <*> o .: "cooldown_expires_at" <*> o .: "redemptions_redeemed_current_stream"
      ChannelPointsCustomRewardRedemptionAddCondition {} -> ChannelPointsCustomRewardRedemptionAdd <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "user_input" <*> o .: "status" <*> o .: "reward" <*> o .: "redeemed_at"
      ChannelPointsCustomRewardRedemptionUpdateCondition {} -> ChannelPointsCustomRewardRedemptionUpdate <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "user_input" <*> o .: "status" <*> o .: "reward" <*> o .: "redeemed_at"
      ChannelPollBeginCondition {} -> ChannelPollBegin <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "title" <*> o .: "choices" <*> o .:? "bits_voting" <*> o .: "channel_points_voting" <*> o .: "started_at" <*> o .: "ends_at"
      ChannelPollProgressCondition {} -> ChannelPollProgress <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "title" <*> o .: "choices" <*> o .:? "bits_voting" <*> o .: "channel_points_voting" <*> o .: "started_at" <*> o .: "ends_at"
      ChannelPollEndCondition {} -> ChannelPollEnd <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "title" <*> o .: "choices" <*> o .:? "bits_voting" <*> o .: "channel_points_voting" <*> o .: "status" <*> o .: "started_at" <*> o .: "ends_at"
      ChannelPredictionBeginCondition {} -> ChannelPredictionBegin <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "title" <*> o .: "outcomes" <*> o .: "started_at" <*> o .: "locks_at"
      ChannelPredictionProgressCondition {} -> ChannelPredictionProgress <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "title" <*> o .: "outcomes" <*> o .: "started_at" <*> o .: "locks_at"
      ChannelPredictionLockCondition {} -> ChannelPredictionLock <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "title" <*> o .: "outcomes" <*> o .: "started_at" <*> o .: "locks_at"
      ChannelPredictionEndCondition {} -> ChannelPredictionEnd <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "title" <*> o .: "outcomes" <*> o .: "started_at" <*> o .: "locks_at"
      DropEntitlementGrantCondition {} -> DropEntitlementGrant <$> o .: "id" <*> o .: "data"
      ExtensionBitsTransactionCreateCondition {} -> ExtensionBitsTransactionCreate <$> o .: "extension_client_id" <*> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "user_input" <*> o .: "product"
      GoalBeginCondition {} -> Goals <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "type" <*> o .: "description" <*> o .: "is_achieved" <*> o .: "current_amount" <*> o .: "target_amount" <*> o .: "started_at" <*> o .: "ended_at"
      GoalProgressCondition {} -> Goals <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "type" <*> o .: "description" <*> o .: "is_achieved" <*> o .: "current_amount" <*> o .: "target_amount" <*> o .: "started_at" <*> o .: "ended_at"
      GoalEndCondition {} -> Goals <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "type" <*> o .: "description" <*> o .: "is_achieved" <*> o .: "current_amount" <*> o .: "target_amount" <*> o .: "started_at" <*> o .: "ended_at"
      HypeTrainBeginCondition {} -> HypeTrainBegin <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "total" <*> o .: "progress" <*> o .: "goal" <*> o .: "top_contributions" <*> o .: "last_contribution" <*> o .: "level" <*> o .: "started_at" <*> o .: "expires_at"
      HypeTrainProgressCondition {} -> HypeTrainProgress <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "level" <*> o .: "total" <*> o .: "progress" <*> o .: "goal" <*> o .: "top_contributions" <*> o .: "last_contribution" <*> o .: "started_at" <*> o .: "expires_at"
      HypeTrainEndCondition {} -> HypeTrainEnd <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "level" <*> o .: "total" <*> o .: "top_contributions" <*> o .: "started_at" <*> o .: "ended_at" <*> o .: "cooldown_ends_at"
      StreamOnlineCondition {} -> StreamOnline <$> o .: "id" <*> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name" <*> o .: "type" <*> o .: "started_at"
      StreamOfflineCondition {} -> StreamOffline <$> o .: "broadcaster_user_id" <*> o .: "broadcaster_user_login" <*> o .: "broadcaster_user_name"
      AuthorizationGrantCondition {} -> AuthorizationGrant <$> o .: "client_id" <*> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name"
      AuthorizationRevokeCondition {} -> AuthorizationRevoke <$> o .: "client_id" <*> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name"
      UserUpdateCondition {} -> UserUpdate <$> o .: "user_id" <*> o .: "user_login" <*> o .: "user_name" <*> o .: "email" <*> o .: "email_verified" <*> o .: "description"
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
