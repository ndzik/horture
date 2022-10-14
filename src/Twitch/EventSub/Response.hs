module Twitch.EventSub.Response
  ( SubscriptionResponse (..),
    ResponseObject (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Twitch.EventSub.Condition
import Twitch.EventSub.Transport

data SubscriptionResponse = SubscriptionResponse
  { subscriptionresponseData :: ![ResponseObject],
    subscriptionresponseTotal :: !Int,
    subscriptionresponseTotalCost :: !Int,
    subscriptionresponseMaxTotalCost :: !Int
  }
  deriving (Show)

data ResponseObject = ResponseObject
  { responseobjectId :: !Text,
    responseobjectStatus :: !Text,
    responseobjectVersion :: !Text,
    responseobjectCondition :: !Condition,
    responseobjectTransport :: !Transport,
    responseobjectCost :: !Int,
    responseobjectCreatedAt :: !Text
  }
  deriving (Show)

instance FromJSON ResponseObject where
  parseJSON v =
    withObject
      "Subscription"
      ( \o ->
          ResponseObject
            <$> o .: "id"
            <*> o .: "status"
            <*> o .: "version"
            <*> parseJSON v
            <*> o .: "transport"
            <*> o .: "cost"
            <*> o .: "created_at"
      )
      v

instance ToJSON ResponseObject where
  toJSON (ResponseObject i s v c t co ca) =
    object
      [ "type" .= conditionTag c,
        "id" .= i,
        "version" .= v,
        "status" .= s,
        "condition" .= c,
        "transport" .= t,
        "cost" .= co,
        "created_at" .= ca
      ]

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("subscriptionresponse_" :: String)) . camelTo2 '_'} ''SubscriptionResponse)
