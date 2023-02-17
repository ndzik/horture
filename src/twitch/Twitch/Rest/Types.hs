module Twitch.Rest.Types
  ( GetCustomRewardsData (..),
    CreateCustomRewardBody (..),
    UpdateCustomRewardBody (..),
    GetUserInformation (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Twitch.EventSub.Image

data GetCustomRewardsData = GetCustomRewardsData
  { getcustomrewardsdataBroadcasterId :: !Text,
    getcustomrewardsdataBroadcasterLogin :: !Text,
    getcustomrewardsdataBroadcasterName :: !Text,
    getcustomrewardsdataId :: !Text,
    getcustomrewardsdataTitle :: !Text,
    getcustomrewardsdataPrompt :: !Text,
    getcustomrewardsdataCost :: !Int,
    getcustomrewardsdataBackgroundColor :: !Text,
    getcustomrewardsdataImage :: !(Maybe Image),
    getcustomrewardsdataDefaultImage :: !Image,
    getcustomrewardsdataIsEnabled :: !Bool,
    getcustomrewardsdataIsUserInputRequired :: !Bool,
    getcustomrewardsdataMaxPerStreamSetting :: !MaxPerStreamSetting,
    getcustomrewardsdataMaxPerUserPerStreamSetting :: !MaxPerUserPerStreamSetting,
    getcustomrewardsdataGlobalCooldownSetting :: !GlobalCooldownSetting,
    getcustomrewardsdataIsPaused :: !Bool,
    getcustomrewardsdataIsInStock :: !Bool,
    getcustomrewardsdataShouldRedemptionsSkipRequestQueue :: !Bool,
    getcustomrewardsdataRedemptionsRedeemedCurrentStream :: !(Maybe Int),
    getcustomrewardsdataCooldownExpiresAt :: !(Maybe Text)
  }
  deriving (Show)

data MaxPerStreamSetting = MaxPerStreamSetting
  { maxperstreamsettingIsEnabled :: !Bool,
    maxperstreamsettingMaxPerStream :: !Int
  }
  deriving (Show)

data MaxPerUserPerStreamSetting = MaxPerUserPerStreamSetting
  { maxperuserperstreamsettingIsEnabled :: !Bool,
    maxperuserperstreamsettingMaxPerUserPerStream :: !Int
  }
  deriving (Show)

data GlobalCooldownSetting = GlobalCooldownSetting
  { globalcooldownsettingIsEnabled :: !Bool,
    globalcooldownsettingGlobalCooldownSeconds :: !Int
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

data UpdateCustomRewardBody = UpdateCustomRewardBody
  { updatecustomrewardbodyTitle :: !(Maybe Text),
    updatecustomrewardbodyCost :: !(Maybe Int),
    updatecustomrewardbodyPrompt :: !(Maybe Text),
    updatecustomrewardbodyIsEnabled :: !(Maybe Bool),
    updatecustomrewardbodyBackgroundColor :: !(Maybe Text),
    updatecustomrewardbodyIsUserInputRequired :: !(Maybe Bool),
    updatecustomrewardbodyIsMaxPerStreamEnabled :: !(Maybe Bool),
    updatecustomrewardbodyMaxPerStream :: !(Maybe Int),
    updatecustomrewardbodyIsMaxPerUserPerStreamEnabled :: !(Maybe Bool),
    updatecustomrewardbodyMaxPerUserPerStream :: !(Maybe Int),
    updatecustomrewardbodyIsGlobalCooldownEnabled :: !(Maybe Bool),
    updatecustomrewardbodyGlobalCooldownSeconds :: !(Maybe Int),
    updatecustomrewardbodyShouldRedemptionsSkipRequestQuee :: !(Maybe Bool)
  } deriving (Show)

data GetUserInformation = GetUserInformation
  { getuserinformationBroadcasterType :: !Text,
    getuserinformationDescription :: !Text,
    getuserinformationDisplayName :: !Text,
    getuserinformationId :: !Text,
    getuserinformationLogin :: !Text,
    getuserinformationOfflineImageUrl :: !Text,
    getuserinformationProfileImageUrl :: !Text,
    getuserinformationType :: !Text,
    getuserinformationViewCount :: !Int,
    getuserinformationEmail :: !(Maybe Text),
    getuserinformationCreatedAt :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("getcustomrewardsdata_" :: String)) . camelTo2 '_'} ''GetCustomRewardsData)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("maxperstreamsetting_" :: String)) . camelTo2 '_'} ''MaxPerStreamSetting)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("maxperuserperstreamsetting_" :: String)) . camelTo2 '_'} ''MaxPerUserPerStreamSetting)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("globalcooldownsetting_" :: String)) . camelTo2 '_'} ''GlobalCooldownSetting)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("getuserinformation_" :: String)) . camelTo2 '_'} ''GetUserInformation)
$( deriveJSON
     defaultOptions
       { fieldLabelModifier = drop (length ("createcustomrewardbody_" :: String)) . camelTo2 '_',
         omitNothingFields = True
       }
     ''CreateCustomRewardBody
 )
$( deriveJSON
     defaultOptions
       { fieldLabelModifier = drop (length ("updatecustomrewardbody_" :: String)) . camelTo2 '_',
         omitNothingFields = True
       }
     ''UpdateCustomRewardBody
 )
