module Twitch.EventSub.EntitlementObject (EntitlementObject (..)) where

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
