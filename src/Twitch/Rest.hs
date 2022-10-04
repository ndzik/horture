{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.Rest (twitchClient, subToWebhook) where

import Data.ByteString hiding (concat)
import Data.Proxy
import Data.Text (Text, concat)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API hiding (addHeader)
import Servant.Client
  ( BaseUrl (..),
    ClientEnv (makeClientRequest),
    ClientM,
    Scheme (..),
    client,
    mkClientEnv,
    runClientM,
  )
import Servant.Client.Core (addHeader)
import Twitch.Types
import Prelude hiding (concat)

type TwitchHelixEventSub c =
  "helix" :> "eventsub" :> "subscriptions" :> ReqBody '[JSON] (WebhookRequest c) :> Post '[JSON] (TwitchSubscriptionResponse c)

-- TODO: Is the port correct?
baseTwitchUrl :: BaseUrl
baseTwitchUrl = BaseUrl Https "api.twitch.tv" 8080 ""

twitchClient :: WebhookRequest RewardRedemptionCondition -> ClientM (TwitchSubscriptionResponse RewardRedemptionCondition)
twitchClient = req
  where
    req = client (Proxy @(TwitchHelixEventSub RewardRedemptionCondition))

subToWebhook :: Text -> Text -> ByteString -> IO ()
subToWebhook broadcasterUserId oauth secret = do
  mgr <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv mgr baseTwitchUrl
      myReachableEndpoint = undefined
      whreq =
        MkWebhookRequest
          { mkwebhookrequestType = "channel.channel_points_custom_reward_redemption.add",
            mkwebhookrequestVersion = "1",
            mkwebhookrequestCondition =
              RewardRedemptionCondition
                { rewardredemptionconditionBroadcasterUserId = broadcasterUserId,
                  rewardredemptionconditionRewardId = Nothing
                },
            mkwebhookrequestTransport =
              Transport
                { transportMethod = "webhook",
                  transportCallback = myReachableEndpoint,
                  transportSecret = Just . decodeUtf8 $ secret
                }
          }
  runClientM (twitchClient whreq) (clientEnv {makeClientRequest = undefined}) >>= print
  where
    addAuth oauth _ req = addHeader "Authorization" (concat ["Bearer ", oauth]) req
