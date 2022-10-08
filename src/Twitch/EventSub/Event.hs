{-# LANGUAGE TemplateHaskell #-}

module Twitch.EventSub.Event
  ( Event (..),
    Reward (..),
    EventStatus (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Text (Text)
import Twitch.EventSub.BitsVoting
import Twitch.EventSub.ChannelPointsVoting
import Twitch.EventSub.Choices
import Twitch.EventSub.EntitlementObject
import Twitch.EventSub.GlobalCooldown
import Twitch.EventSub.Image
import Twitch.EventSub.Contribution
import Twitch.EventSub.Message
import Twitch.EventSub.Outcomes
import Twitch.EventSub.Poll
import Twitch.EventSub.Product
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
        viewer :: !Int
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

$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''EventStatus)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("rreward_" :: String)) . camelTo2 '_'} ''Reward)
