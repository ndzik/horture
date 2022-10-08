{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Twitch.Types
  ( NotificationReq (..),
    Subscription (..),
    Status (..),
    ChannelRedemptionNotification (..),
    Verification (..),
    RedemptionRewardEvent (..),
    EventStatus (..),
    Reward (..),
    ResponseObject (..),
    SubscriptionResponse (..),
    ClientCredentialRequest (..),
    ClientCredentialResponse (..),
    AccessTokenScopesRequest (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text, unwords)
import Twitch.EventSub
import Twitch.EventSub.Event
import Web.FormUrlEncoded (FromForm (..), ToForm (..), parseUnique)
import Web.HttpApiData (ToHttpApiData, toQueryParam)
import Prelude hiding (unwords)

data NotificationEvent = NotificationEvent
  { notificationeventSubscription :: !Subscription,
    notificationeventEvent :: !Event
  }
  deriving (Show)

data Verification = Verification
  { verificationChallenge :: !Text,
    verificationSubscription :: !Subscription
  }
  deriving (Show)

data NotificationReq = NotificationReq
  { notificationreqNotification :: !Text,
    notificationreqWebhookCallbackVerification :: !Text,
    notificationreqRevocation :: !Text
  }
  deriving (Show)

-- Subscription is the webhook subscription event type received from twitch
-- when requesting a webhook subscription.
data Subscription = Subscription
  { subscriptionId :: !Text,
    subscriptionVersion :: !Text,
    subscriptionStatus :: !Status,
    subscriptionCost :: !Int,
    subscriptionCondition :: !Condition,
    subscriptionTransport :: !Transport,
    subscriptionCreatedAt :: !Text
  }
  deriving (Show)

instance FromJSON Subscription where
  parseJSON v =
    withObject
      "Subscription"
      ( \o ->
          Subscription
            <$> o .: "id"
            <*> o .: "version"
            <*> o .: "status"
            <*> o .: "cost"
            <*> parseJSON v
            <*> o .: "transport"
            <*> o .: "created_at"
      )
      v

instance ToJSON Subscription where
  toJSON (Subscription i v s c con tr ca) =
    object
      [ "type" .= conditionTag con,
        "id" .= i,
        "version" .= v,
        "status" .= s,
        "cost" .= c,
        "condition" .= con,
        "transport" .= tr,
        "created_at" .= ca
      ]

data Status
  = Enabled
  | Disabled
  | WebhookCallbackVerificationPending
  deriving (Show)

data ChannelRedemptionNotification = ChannelRedemptionNotification
  { channelredemptionnotificationSubscription :: !Subscription,
    channelredemptionnotificationEvent :: !Event
  }
  deriving (Show)

data ClientCredentialRequest = ClientCredentialRequest
  { clientcredentialrequestClientId :: !Text,
    clientcredentialrequestClientSecret :: !Text,
    clientcredentialrequestGrantType :: !Text
  }
  deriving (Show)

instance FromForm ClientCredentialRequest where
  fromForm f =
    ClientCredentialRequest
      <$> parseUnique "client_id" f
      <*> parseUnique "client_secret" f
      <*> parseUnique "grant_type" f

instance ToForm ClientCredentialRequest where
  toForm ccr =
    [ ("client_id", toQueryParam (clientcredentialrequestClientId ccr)),
      ("client_secret", toQueryParam (clientcredentialrequestClientSecret ccr)),
      ("grant_type", toQueryParam (clientcredentialrequestGrantType ccr))
    ]

data ClientCredentialResponse = ClientCredentialResponse
  { clientcredentialresponseAccessToken :: !Text,
    clientcredentialresponseExpiresIn :: !Int,
    clientcredentialresponseTokenType :: !Text
  }
  deriving (Show)

newtype AccessTokenScopesRequest = AccessTokenScopesRequest [Text] deriving (Show)

instance ToHttpApiData AccessTokenScopesRequest where
  toQueryParam (AccessTokenScopesRequest ss) = toQueryParam . unwords $ ss

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("verification_" :: String)) . camelTo2 '_'} ''Verification)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("channelredemptionnotification_" :: String)) . camelTo2 '_'} ''ChannelRedemptionNotification)
$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("clientcredentialresponse_" :: String)) . camelTo2 '_'} ''ClientCredentialResponse)
$(deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''Status)
