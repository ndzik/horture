{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Horture.Server.Websocket
  ( handleWebsocketConn,
  )
where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Chan.Synchronous
import Control.Exception (catch)
import Control.Lens
import Control.Monad.RWS
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Default
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

handleWebsocketConn :: ByteString -> Text -> Text -> BaseUrl -> Chan Twitch.EventNotification -> AuthorizationToken -> Application
handleWebsocketConn secret cid cb endpoint tevChan aat = websocketsOr defaultConnectionOptions (runHortureClientWS hortureClientConn secret cid cb endpoint tevChan aat) invalidWSApplication
  where
    invalidWSApplication :: Application
    invalidWSApplication _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | Client connection from the POV of the horture server. This is NOT the
-- client, that should be used if one wants to interact with the server from
-- the outside. For this use the dedicated Horture.Client!
hortureClientConn :: HortureClient ()
hortureClientConn = do
  conn <- asks _conn
  twitchMessages <- asks _twitchEventQueue
  env <- ask
  let readerAction = do
        liftIO (receiveData @HortureClientMessage conn) >>= handleClientMessage
        readerAction
      writerAction = do
        logFM DebugS "Waiting for TwitchEvent..."
        -- We cannot use a blocking `readChan` call here. Most likely because of
        -- https://hackage.haskell.org/package/synchronous-channels-0.2/docs/Control-Concurrent-Chan-Synchronous.html
        -- use of `uninterruptableMask` in `readChan`.
        -- TODO: Remove `synchronous-channels` as a dependency and use either
        -- STM or a different dependency.
        liftIO (maybeTry (tryReadChan twitchMessages)) >>= \case
          Nothing -> liftIO (threadDelay 250_000) >> writerAction
          Just msg -> do
            logFM DebugS "TwitchEvent encountered, forwarding..."
            -- Forward twitch event to connected client.
            liftIO (sendTextData @HortureServerMessage conn (HortureEventSub msg))
            writerAction

  fullIfDone <- liftIO newEmptyMVar
  readerTID <- liftIO . forkIO $ do
    void (evalRWST (unHortureClient readerAction) env def)
      `catch` \(_ :: ConnectionException) -> do
        putMVar fullIfDone ()
  writerTID <- liftIO . forkIO $ do
    void (evalRWST (unHortureClient writerAction) env def)
      `catch` \(_ :: ConnectionException) -> do
        putMVar fullIfDone ()
  void . liftIO . takeMVar $ fullIfDone
  void . liftIO . mapM_ killThread $ [readerTID, writerTID]
  logFM InfoS "WebSocketClient terminated"

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
  case res of
    Left err -> do
      logFM ErrorS . logStr $ show err
      closeClientConn
    Right sr -> do
      logFM InfoS . logStr $ "subscribed for websocketconnection to twitch for clientid: " <> unpack buid
      logFM DebugS . logStr $ encode sr
handleClientMessage (HortureUpdate uat) = updateToken uat

closeClientConn :: HortureClient ()
closeClientConn = asks (^. conn) >>= liftIO . flip sendClose HortureGarbage

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
