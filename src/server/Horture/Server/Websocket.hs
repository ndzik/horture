{-# LANGUAGE LambdaCase #-}

module Horture.Server.Websocket
  ( handleWebsocketConn,
    hortureWS,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Synchronous
import Control.Monad.RWS
import Data.Default
import Data.Functor ((<&>))
import Data.Text (Text)
import Horture.Server.Message
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.WebSockets hiding (Request, Response, requestHeaders)
import qualified Twitch.EventSub.Event as Twitch
import Prelude hiding (concat)

handleWebsocketConn :: Chan Twitch.Event -> Text -> Application
handleWebsocketConn tevChan aat = websocketsOr defaultConnectionOptions (hortureWS tevChan aat) invalidWSApplication
  where
    invalidWSApplication :: Application
    invalidWSApplication _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | hortureWSApp will forward and multiplex received webhook messages to a
-- connected client.
hortureWS :: Chan Twitch.Event -> Text -> ServerApp
hortureWS tevChan aat pendingConn = do
  conn <- acceptRequest pendingConn
  chan <- newChan @HortureClientMessage
  -- TODO: What should I do with the ThreadID? Cache and terminate externally?
  void . forkIO $ let loop = receiveData @HortureClientMessage conn >>= writeChan chan >> loop in loop
  let conf =
        HortureClientConfig
          { _conn = conn,
            _messageQueue = chan,
            _twitchEventQueue = tevChan,
            _appAccess = aat
          }
  evalRWST hortureClientConn conf def <&> fst

data HortureClientConfig = HortureClientConfig
  { _conn :: !Connection,
    _messageQueue :: !(Chan HortureClientMessage),
    _twitchEventQueue :: !(Chan Twitch.Event),
    _appAccess :: !Text
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
  forever $ do
    liftIO (tryReadChan clientMessages) >>= \case
      Success cm -> handleClientMessage cm
      _otherwise -> return ()
    -- Blocking read on twitchMessages.
    msg <- liftIO (HortureEventSub <$> readChan twitchMessages)
    -- Forward twitch event to connected client.
    liftIO (sendTextData @HortureServerMessage conn msg)

handleClientMessage :: HortureClientMessage -> HortureClient ()
handleClientMessage HortureGarbage = return ()
handleClientMessage (HortureAuthorization uat) = updateToken uat
handleClientMessage (HortureUpdate uat) = updateToken uat

-- | Update tokens used by the external server to respond to EventSub
-- notifications.
--
-- TODO: Set up the subscription when tokens are updated.
updateToken :: Text -> HortureClient ()
updateToken uat = modify (\hs -> hs {_hcsuserAccessToken = Just uat})
