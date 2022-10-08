{-# LANGUAGE OverloadedStrings #-}

module Twitch.EventSub.Request
  ( WebhookRequest (..),
    Transport (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import Twitch.EventSub.Condition
import Twitch.EventSub.Transport

data WebhookRequest = WebhookRequest
  { webhookrequestVersion :: !Text,
    webhookrequestCondition :: !Condition,
    webhookrequestTransport :: !Transport
  }
  deriving (Show)

instance FromJSON WebhookRequest where
  parseJSON v =
    withObject
      "WebhookRequest"
      ( \o ->
          WebhookRequest <$> o .: "version" <*> parseJSON v <*> o .: "transport"
      )
      v

instance ToJSON WebhookRequest where
  toJSON (WebhookRequest v c t) = object ["type" .= conditionTag c, "version" .= v, "condition" .= c, "transport" .= t]
