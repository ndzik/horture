module Twitch.Rest.Types
  ( GetCustomRewardsData (..),
    CreateCustomRewardBody (..),
    GetUserInformation (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Twitch.EventSub.GlobalCooldown
import Twitch.EventSub.Image
import Twitch.EventSub.Reward

data GetCustomRewardsData = GetCustomRewardsData
  { getcustomrewardsdataBroadcasterId :: !Text,
    getcustomrewardsdataBroadcasterLogin :: !Text,
    getcustomrewardsdataBroadcasterName :: !Text,
    getcustomrewardsdataId :: !Text,
    getcustomrewardsdataTitle :: !Text,
    getcustomrewardsdataPrompt :: !Text,
    getcustomrewardsdataCost :: !Int,
    getcustomrewardsdataImage :: !(Maybe Image),
    getcustomrewardsdataDefaultImage :: !Image,
    getcustomrewardsdataIsEnabled :: !Bool,
    getcustomrewardsdataIsUserInputRequired :: !Text,
    getcustomrewardsdataMaxPerStreamSetting :: !MaxPerStream,
    getcustomrewardsdataMaxPerUserPerStreamSetting :: !MaxPerUserPerStream,
    getcustomrewardsdataGlobalCooldownSetting :: !GlobalCooldown,
    getcustomrewardsdataIsPaused :: !Bool,
    getcustomrewardsdataIsInStock :: !Bool,
    getcustomrewardsdataShouldRedemptionsSkipRequestQueue :: !Bool,
    getcustomrewardsdataRedemptionsRedeemedCurrentStream :: !Int,
    getcustomrewardsdataCooldownExpiresAt :: !Text
  }
  deriving (Show)

data CreateCustomRewardBody = CreateCustomRewardBody
  { createcustomrewardbodyTitle :: !Text,
    createcustomrewardbodyCost :: !Int,
    createcustomrewardbodyPrompt :: !(Maybe Text),
    createcustomrewardbodyIsEnabled :: !(Maybe Bool),
    createcustomrewardbodyBackgroundColor :: !(Maybe Text),
    createcustomrewardbodyIsUserInputRequired :: !(Maybe Bool),
    createcustomrewardbodyIsMaxPerStreamEnabled :: !(Maybe Bool),
    createcustomrewardbodyMaxPerStream :: !(Maybe Int),
    createcustomrewardbodyIsMaxPerUserPerStreamEnabled :: !(Maybe Bool),
    createcustomrewardbodyMaxPerUserPerStream :: !(Maybe Int),
    createcustomrewardbodyIsGlobalCooldownEnabled :: !(Maybe Bool),
    createcustomrewardbodyGlobalCooldownSeconds :: !(Maybe Int),
    createcustomrewardbodyShouldRedemptionsSkipRequestQuee :: !(Maybe Bool)
  }
  deriving (Show)

data GetUserInformation = GetUserInformation
  { getuserinformationBroadcasterType :: !Text
  , getuserinformationDescription :: !Text
  , getuserinformationDisplayName :: !Text
  , getuserinformationId :: !Text
  , getuserinformationLogin :: !Text
  , getuserinformationOfflineImageUrl :: !Text
  , getuserinformationProfileImageUrl :: !Text
  , getuserinformationType :: !Text
  , getuserinformationViewCount :: !Int
  , getuserinformationEmail :: !(Maybe Text)
  , getuserinformationCreatedAt :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("getcustomrewardsdata_" :: String)) . camelTo2 '_'} ''GetCustomRewardsData)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("getuserinformation_" :: String)) . camelTo2 '_'} ''GetUserInformation)
$( deriveJSON
     defaultOptions
       { fieldLabelModifier = drop (length ("createcustomrewardbody_" :: String)) . camelTo2 '_',
         omitNothingFields = True
       }
     ''CreateCustomRewardBody
 )
