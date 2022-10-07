{-# LANGUAGE TemplateHaskell #-}

-- | Condition types according to the twitch specification:
--
-- https://dev.twitch.tv/docs/eventsub/eventsub-reference#conditions
module Twitch.Types.EventSub.Condition
  ( ChannelRaid (..),
    ChannelPointsCustomReward (..),
    ChannelPoll (..),
    ChannelPrediction (..),
    ChannelFollow (..),
    ChannelUpdate (..),
    ChannelCheer (..),
    ChannelSubscription (..),
    ChannelBan (..),
    ChannelModerator (..),
    Goals (..),
    UserUpdate (..),
    Stream (..),
    HypeTrain (..),
    Authorization (..),
    DropEntitlementGrant (..),
    ExtensionBitsTransactionCreate (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Prelude hiding (unwords)

data ChannelPointsCustomReward = ChannelPointsCustomReward
  { channelpointscustomrewardBroadcasterUserId :: !Text,
    channelpointscustomrewardRewardId :: !(Maybe Text)
  }
  deriving (Show)

newtype ChannelPoll = ChannelPoll
  { channelpollBroadcasterUserId :: Text
  }
  deriving (Show)

newtype ChannelPrediction = ChannelPrediction
  { channelpredictionUserId :: Text
  }
  deriving (Show)

data ChannelRaid = ChannelRaid
  { channelraidFromBroadcasterUserId :: !(Maybe Text),
    channelraidToBroadcasterUserId :: !(Maybe Text)
  }
  deriving (Show)

data DropEntitlementGrant = DropEntitlementGrant
  { dropentitlementgrantOrganizationId :: !Text,
    dropentitlementgrantCategoryId :: !(Maybe Text),
    dropentitlementgrantCampaignId :: !(Maybe Text)
  }
  deriving (Show)

newtype ChannelFollow = ChannelFollow
  { channelfollowBroadcasterUserId :: Text
  }
  deriving (Show)

newtype ChannelUpdate = ChannelUpdate
  { channelupdateBroadcasterUserId :: Text
  }
  deriving (Show)

newtype ChannelCheer = ChannelCheer
  { channelcheerBroadcasterUserId :: Text
  }
  deriving (Show)

newtype ChannelSubscription = ChannelSubscription
  { channelsubscriptionBroadcasterUserId :: Text
  }
  deriving (Show)

newtype ChannelBan = ChannelBan
  { channelbanBroadcasterUserId :: Text
  }
  deriving (Show)

newtype ChannelModerator = ChannelModerator
  { channelmoderatorBroadcasterUserId :: Text
  }
  deriving (Show)

newtype Goals = Goals
  { goalsBroadcasterUserId :: Text
  }
  deriving (Show)

newtype HypeTrain = HypeTrain
  { hypetrainBroadcasterUserId :: Text
  }
  deriving (Show)

newtype Stream = Stream
  { streamBroadcasterUserId :: Text
  }
  deriving (Show)

newtype Authorization = Authorization
  { authorizationClientId :: Text
  }
  deriving (Show)

newtype UserUpdate = UserUpdate
  { userupdateUserId :: Text
  }
  deriving (Show)

newtype ExtensionBitsTransactionCreate = ExtensionBitsTransactionCreate
  { extensionbitstransactioncreateExtensionClientId :: Text
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelraid_" :: String)) . camelTo2 '_'} ''ChannelRaid)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("goals_" :: String)) . camelTo2 '_'} 'Goals)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("authorization_" :: String)) . camelTo2 '_'} 'Authorization)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("stream" :: String)) . camelTo2 '_'} 'Stream)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("hypetrain_" :: String)) . camelTo2 '_'} 'HypeTrain)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("extensionbitstransactioncreate_" :: String)) . camelTo2 '_'} 'ExtensionBitsTransactionCreate)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelpointscustomreward_" :: String)) . camelTo2 '_'} 'ChannelPointsCustomReward)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelpoll_" :: String)) . camelTo2 '_'} 'ChannelPoll)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelprediction_" :: String)) . camelTo2 '_'} 'ChannelPrediction)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelfollow_" :: String)) . camelTo2 '_'} 'ChannelFollow)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelupdate_" :: String)) . camelTo2 '_'} 'ChannelUpdate)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelcheer_" :: String)) . camelTo2 '_'} 'ChannelCheer)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelsubscription_" :: String)) . camelTo2 '_'} 'ChannelSubscription)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelban_" :: String)) . camelTo2 '_'} 'ChannelBan)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelmoderator_" :: String)) . camelTo2 '_'} 'ChannelModerator)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("dropentitlementgrant_" :: String)) . camelTo2 '_'} 'DropEntitlementGrant)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("userupdate_" :: String)) . camelTo2 '_'} 'UserUpdate)
