{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.Rest (reqWebhook) where

import Data.Proxy
import Servant.API
import Servant.Client
import Twitch.Types

type TwitchHelixEventSub c =
  "helix" :> "eventsub" :> "subscriptions" :> ReqBody '[JSON] (WebhookRequest c) :> Post '[JSON] NoContent

-- TODO: Is the port correct?
baseTwitchUrl :: BaseUrl
baseTwitchUrl = BaseUrl Https "api.twitch.tv" 8080 ""

reqWebhook :: WebhookRequest RewardRedemptionCondition -> ClientM NoContent
reqWebhook = req
  where
    req = client (Proxy @(TwitchHelixEventSub RewardRedemptionCondition))
