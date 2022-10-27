{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Horture.Server.Websocket
  ( handleWebsocketConn,
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Monad.RWS
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Horture.Server.Client
import Horture.Server.Message
import Katip
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.WebSockets hiding (Request, Response, requestHeaders)
import Servant.Client
import Twitch.EventSub
import qualified Twitch.EventSub.Notification as Twitch
import Twitch.Rest
import Prelude hiding (concat)

handleWebsocketConn :: ByteString -> Text -> Text -> Chan Twitch.EventNotification -> AuthorizationToken -> Application
handleWebsocketConn secret cid cb tevChan aat = websocketsOr defaultConnectionOptions (runHortureClientWS hortureClientConn secret cid cb tevChan aat) invalidWSApplication
  where
    invalidWSApplication :: Application
    invalidWSApplication _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | Client connection from the POV of the horture server. This is NOT the
-- client, that should be used if one wants to interact with the server from
-- the outside. For this use the dedicated Horture.Client!
hortureClientConn :: HortureClient ()
hortureClientConn = do
  conn <- asks _conn
  clientMessages <- asks _messageQueue
  twitchMessages <- asks _twitchEventQueue
  liftIO (readChan clientMessages) >>= handleClientMessage
  forever $ do
    logFM DebugS "Waiting for TwitchEvent..."
    -- Blocking read on twitchMessages.
    msg <- liftIO (HortureEventSub <$> readChan twitchMessages)
    logFM DebugS "TwitchEvent encountered, forwarding..."
    -- Forward twitch event to connected client.
    liftIO (sendTextData @HortureServerMessage conn msg)

handleClientMessage :: HortureClientMessage -> HortureClient ()
handleClientMessage HortureGarbage = do
  logFM DebugS "Received garbage over WebSocket"
  return ()
handleClientMessage (HortureAuthorization buid) = do
  logFM InfoS . logStr $ "Received authorization for userid: " <> show buid
  mgr <- liftIO $ newManager tlsManagerSettings
  at <- asks _appAccess
  twitchEndpoint <- asks _twitchApiEndpoint
  secret <- asks _secret
  clientId <- asks _appClientId
  apiCallbackUrl <- asks _twitchApiCallback
  let cl@TwitchEventSubClient {eventsubSubscribe} = twitchEventSubClient clientId at
      clientEnv = mkClientEnv mgr twitchEndpoint
  removeAllOldSubscriptions cl clientEnv
  res <-
    liftIO $
      runClientM
        ( eventsubSubscribe
            ( WebhookRequest
                "1"
                ( ChannelPointsCustomRewardRedemptionAddCondition
                    { broadcasterUserId = buid,
                      rewardId = Nothing
                    }
                )
                (Transport "webhook" apiCallbackUrl (Just . decodeUtf8 $ secret))
            )
        )
        clientEnv
  logFM InfoS . logStr $ "subscribed for websocketconnection to twitch for clientid: " <> unpack buid
  logFM DebugS . logStr $ show res
handleClientMessage (HortureUpdate uat) = updateToken uat

removeAllOldSubscriptions :: TwitchEventSubClient -> ClientEnv -> HortureClient ()
removeAllOldSubscriptions TwitchEventSubClient {..} env = do
  liftIO (runClientM (eventsubGetSubscriptions (Just "enabled")) env) >>= \case
      Left err -> logFM WarningS . logStr $ "Fetching existing subscriptions failed: " <> show err
      Right (SubscriptionResponse subs _ _ _) ->
        mapM_
          ( \ResponseObject {responseobjectId} ->
              liftIO $ runClientM (eventsubDeleteSubscription responseobjectId) env
          )
          subs

-- | Update tokens used by the external server to respond to EventSub
-- notifications.
--
-- TODO: Set up the subscription when tokens are updated.
updateToken :: Text -> HortureClient ()
updateToken uat = modify (\hs -> hs {_hcsuserAccessToken = Just uat})
