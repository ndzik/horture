module Twitch.EventSub.EntitlementObject (EntitlementObject (..)) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

data EntitlementObject = EntitlementObject
  { entitlementOrganizationId :: !Text,
    entitlementCategoryId :: !Text,
    entitlementCategoryName :: !Text,
    entitlementCampaignId :: !Text,
    entitlementUserId :: !Text,
    entitlementUserName :: !Text,
    entitlementUserLogin :: !Text,
    entitlementEntitlementId :: !Text,
    entitlementBenefitId :: !Text,
    entitlementCreatedAt :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("entitlement_" :: String)) . camelTo2 '_'} ''EntitlementObject)
