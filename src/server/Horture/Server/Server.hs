module Horture.Server.Server (runHorture) where

import Control.Concurrent.Chan.Synchronous
import Control.Exception (bracket)
import Control.Lens
import Control.Monad.Reader
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.Aeson
import Data.ByteArray.Encoding
import Data.ByteString (ByteString, concat)
import Data.Maybe
import Data.Text (pack)
import Data.Text.Encoding
import Horture.Server.Config
import Horture.Server.Reader
import Horture.Server.Websocket
import Katip
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant.Client (showBaseUrl)
import System.IO (stdout)
import System.Random.Stateful
import qualified Twitch.EventSub.Notification as Twitch
import Twitch.Rest (AuthorizationToken (..))
import Prelude hiding (concat)

-- | runHorture runs the horture server which is exposing the
-- <your-domain>/eventsub endpoint. This can only be used with the production
-- twitch api if served in conjunction with a SSL termination proxy from port
-- 443 to your local port.
runHorture :: HortureServerConfig -> IO ()
runHorture conf = do
  secret <- encodeUtf8 . pack . show <$> uniformByteStringM 16 globalStdGen
  tevChan <- newChan @Twitch.EventNotification
  case (_keyFile conf, _certFile conf) of
    (Just kf, Just cf) -> runTLS (tlsSettings cf kf) defaultSettings . runHortureServer conf tevChan $ secret
    _otherwise -> run (_port conf) . runHortureServer conf tevChan $ secret

type HortureServer a = ReaderT HortureServerEnv IO a

type HortureHandler' = Request -> (Response -> IO ResponseReceived) -> HortureServer ResponseReceived

type HortureHandler = Request -> (Response -> HortureServer ResponseReceived) -> HortureServer ResponseReceived

runHortureServer :: HortureServerConfig -> Chan Twitch.EventNotification -> ByteString -> Application
runHortureServer cfg chan s req respondWith = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "horture-server" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    let env =
          HortureServerEnv
            { _conf = cfg,
              _notificationChan = chan,
              _secret = s,
              _logEnv = le,
              _logContext = mempty,
              _logNamespace = mempty
            }
    runReaderT (hortureApp req (liftIO . respondWith)) env

hortureApp :: HortureHandler'
hortureApp req respondWith = do
  -- _conf tevChan secret req respondWith = do
  case pathInfo req of
    -- TODO: Split the singleton `eventsub` endpoint up, s.t. it is
    -- dynamically created after a user has authorized via a websocket
    -- connection. We will use the singleton for now, since this is a MVP.
    ["eventsub"] -> handleTwitchNotification req (liftIO . respondWith)
    ["ws"] -> do
      -- print @String "Received request on WS endpoint"
      s <- asks (^. secret)
      clientId <- asks (^. conf . appClientId)
      cb <- asks (^. conf . callback)
      chan <- asks (^. notificationChan)
      token <- asks (^. conf . appToken)
      liftIO $
        handleWebsocketConn
          s
          clientId
          (pack . showBaseUrl $ cb)
          chan
          (AuthorizationToken token)
          req
          respondWith
    _otherwise -> liftIO $ respondWith notFound

handleTwitchNotification :: HortureHandler
handleTwitchNotification req respondWith = do
  -- print @String "TwitchNotification called."
  if not . isPOST . requestMethod $ req
    then do
      -- print @String "Notification is not POST"
      respondWith methodNotAllowed
    else do
      body <- liftIO $ getRequestBodyChunk req
      s <- asks (^. secret)
      -- print @String "Notification is POST"
      let headers = requestHeaders req
      res <- case lookup "Twitch-Eventsub-Message-Signature" headers of
        Just sig -> verifyFromTwitch req body s sig
        _otherwise -> do
          return False
      if not res
        then do
          -- print @String "Invalid Twitch-Signature in message: "
          -- print body
          respondWith badRequest
        else case lookup "Twitch-Eventsub-Message-Type" $ requestHeaders req of
          Just messageType -> do
            chan <- asks (^. notificationChan)
            handleMessageType body chan messageType req >>= respondWith
          _otherwise -> do
            -- print @String "Received Non-Eventsub message:"
            -- print _otherwise
            respondWith badRequest

verifyFromTwitch :: Request -> ByteString -> ByteString -> ByteString -> HortureServer Bool
verifyFromTwitch req body secret twitchSig = do
  let headers = requestHeaders req
      id = lookup "Twitch-Eventsub-Message-Id" headers
      timestamp = lookup "Twitch-Eventsub-Message-Timestamp" headers
  let hmacMsg = concat3 <$> id <*> timestamp <*> Just body
      hmacHex = createHmacHex secret <$> hmacMsg
      isValid = verifySignature twitchSig <$> hmacHex
  case isValid of
    Nothing -> return False
    Just r -> return r
  where
    concat3 msg1 msg2 msg3 = concat [msg1, msg2, msg3]

{- HLINT ignore "Use ++"-}
createHmacHex :: ByteString -> ByteString -> ByteString
createHmacHex secret hmacMsg = concat ["sha256=", convertToBase Base16 (hmac secret hmacMsg :: HMAC SHA256)]

-- NOTE: The equalitiy function here should probably be hardened against timing
-- attacks.
verifySignature :: ByteString -> ByteString -> Bool
verifySignature expectedSig actualSig = expectedSig == actualSig

handleMessageType :: ByteString -> Chan Twitch.EventNotification -> ByteString -> Request -> HortureServer Response
handleMessageType body _ "webhook_callback_verification" _req = do
  -- print @String "CallbackVerification challenge received"
  handleCallbackVerification body
handleMessageType body tevChan "notification" _req = do
  -- print @String "NotificationEvent received"
  handleNotification tevChan body
handleMessageType _body _ "revocation" req = do
  -- print @String "RevocationEvent received"
  handleRevocation req
handleMessageType _body _ _othertype _req = do
  -- print $ "Received othertype" <> othertype
  return okNoContent

handleCallbackVerification :: ByteString -> HortureServer Response
handleCallbackVerification body = do
  let mChallenge = decodeStrict @Twitch.ChallengeNotification body
  -- case mChallenge of
  --   Nothing -> print @String "Unable to decode challenge body"
  --   Just chall -> print . encode $ chall
  return $ maybe badRequestBody respondWithChallenge mChallenge
  where
    respondWithChallenge mChallenge =
      responseBuilder status200 [("Content-Type", "text/plain")]
        . encodeUtf8Builder
        . Twitch.challengenotificationChallenge
        $ mChallenge

handleNotification :: Chan Twitch.EventNotification -> ByteString -> HortureServer Response
handleNotification tevChan body = do
  -- print body
  case decodeStrict @Twitch.EventNotification body of
    Nothing -> return () -- void $ print @String "unable to decode Twitch.EventNotification"
    Just ev -> liftIO $ writeChan tevChan ev
  -- Always returning okNoContent, since we cannot ask twitch to resend this
  -- message if ill-formatted.
  return okNoContent

-- TODO: Resubscribe to twitch events of interest here.
handleRevocation :: Request -> HortureServer Response
handleRevocation _req = return okNoContent

okNoContent :: Response
okNoContent = responseBuilder status204 [] mempty

notFound :: Response
notFound = responseBuilder status404 [("Content-Type", "text/plain")] mempty

badRequest :: Response
badRequest = responseBuilder status400 [("Content-Type", "text/plain")] mempty

badRequestBody :: Response
badRequestBody = responseBuilder status422 [("Content-Type", "text/plain")] mempty

methodNotAllowed :: Response
methodNotAllowed = responseBuilder status405 [("Content-Type", "text/plain")] mempty

isPOST :: ByteString -> Bool
isPOST "POST" = True
isPOST _ = False
