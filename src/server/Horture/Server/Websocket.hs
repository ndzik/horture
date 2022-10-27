{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Horture.Server.Websocket
  ( handleWebsocketConn,
    hortureWS,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Synchronous
import Control.Monad.RWS
import Data.ByteString (ByteString)
import Data.Default
import Data.Functor ((<&>))
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Horture.Server.Message
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
handleWebsocketConn secret cid cb tevChan aat = websocketsOr defaultConnectionOptions (hortureWS secret cid cb tevChan aat) invalidWSApplication
  where
    invalidWSApplication :: Application
    invalidWSApplication _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | hortureWSApp will forward and multiplex received webhook messages to a
-- connected client.
hortureWS :: ByteString -> Text -> Text -> Chan Twitch.EventNotification -> AuthorizationToken -> ServerApp
hortureWS secret cid cb tevChan aat pendingConn = do
  conn <- acceptRequest pendingConn
  chan <- newChan @HortureClientMessage
  -- TODO: What should I do with the ThreadID? Cache and terminate externally?
  void . forkIO $ let loop = receiveData @HortureClientMessage conn >>= writeChan chan >> loop in loop
  let conf =
        HortureClientConfig
          { _conn = conn,
            _secret = secret,
            _twitchApiEndpoint = BaseUrl Http "localhost" 8080 "mock",
            _messageQueue = chan,
            _twitchEventQueue = tevChan,
            _twitchApiCallback = cb,
            _appClientId = cid,
            _appAccess = aat
          }
  print @String "Starting client connection handling"
  evalRWST hortureClientConn conf def <&> fst

data HortureClientConfig = HortureClientConfig
  { _conn :: !Connection,
    _secret :: !ByteString,
    _twitchApiEndpoint :: !BaseUrl,
    _twitchApiCallback :: !Text,
    _messageQueue :: !(Chan HortureClientMessage),
    _twitchEventQueue :: !(Chan Twitch.EventNotification),
    _appClientId :: !Text,
    _appAccess :: !AuthorizationToken
  }

data HortureClientState = HortureClientState
  { _hcsuserAccessToken :: !(Maybe Text),
    _hcsappAccessToken :: !(Maybe Text)
  }
  deriving (Show)

instance Default HortureClientState where
  def =
    HortureClientState
      { _hcsuserAccessToken = Nothing,
        _hcsappAccessToken = Nothing
      }

type HortureClient a = RWST HortureClientConfig () HortureClientState IO a

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
    liftIO (print @String "Waiting for TwitchEvent...")
    -- Blocking read on twitchMessages.
    msg <- liftIO (HortureEventSub <$> readChan twitchMessages)
    liftIO (print @String "TwitchEvent encountered, forwarding...")
    -- Forward twitch event to connected client.
    liftIO (sendTextData @HortureServerMessage conn msg)

handleClientMessage :: HortureClientMessage -> HortureClient ()
handleClientMessage HortureGarbage = do
  liftIO $ print @String "Received garbage over WebSocket"
  return ()
handleClientMessage (HortureAuthorization buid) = do
  liftIO . print $ "Received authorization for userid: " <> show buid
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
  liftIO . print @String $ "subscribed for websocketconnection to twitch for clientid: " <> unpack buid
  liftIO $ print res
handleClientMessage (HortureUpdate uat) = updateToken uat

removeAllOldSubscriptions :: TwitchEventSubClient -> ClientEnv -> HortureClient ()
removeAllOldSubscriptions TwitchEventSubClient {..} env = do
  liftIO $
    runClientM (eventsubGetSubscriptions (Just "enabled")) env >>= \case
      Left err -> print @String "Fetching existing subscriptions failed: " >> print err
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
