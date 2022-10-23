module Horture.Server.Server (runHortureServer) where

import Control.Concurrent.Chan.Synchronous
import Control.Monad (void)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.Aeson
import Data.ByteArray.Encoding
import Data.ByteString (ByteString, concat)
import Data.Maybe
import Data.Text (pack)
import Data.Text.Encoding
import Horture.Server.Config
import Horture.Server.Websocket
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant.Client (showBaseUrl)
import System.Random.Stateful
import qualified Twitch.EventSub.Notification as Twitch
import Twitch.Rest (AuthorizationToken (..))
import Prelude hiding (concat)

-- | runHortureServer runs the horture server which is exposing the
-- <your-domain>/eventsub endpoint. This can only be used with the production
-- twitch api if served in conjunction with a SSL termination proxy from port
-- 443 to your local port.
runHortureServer :: HortureServerConfig -> IO ()
runHortureServer conf = do
  secret <- encodeUtf8 . pack . show <$> uniformByteStringM 16 globalStdGen
  tevChan <- newChan @Twitch.EventNotification
  case (_keyFile conf, _certFile conf) of
    (Just kf, Just cf) -> runTLS (tlsSettings cf kf) defaultSettings . hortureApp conf tevChan $ secret
    _otherwise -> run (_port conf) . hortureApp conf tevChan $ secret

hortureApp :: HortureServerConfig -> Chan Twitch.EventNotification -> ByteString -> Application
hortureApp _conf tevChan secret req respondWith = do
  case pathInfo req of
    -- TODO: Split the singleton `eventsub` endpoint up, s.t. it is
    -- dynamically created after a user has authorized via a websocket
    -- connection. We will use the singleton for now, since this is a MVP.
    ["eventsub"] -> handleTwitchNotification tevChan secret req respondWith
    ["ws"] -> do
      print @String "Received request on WS endpoint"
      handleWebsocketConn secret (_appClientId _conf) (pack . showBaseUrl $ _callback _conf) tevChan (AuthorizationToken $ _appToken _conf) req respondWith
    _otherwise -> respondWith notFound

handleTwitchNotification :: Chan Twitch.EventNotification -> ByteString -> Application
handleTwitchNotification tevChan secret req respondWith = do
  print @String "TwitchNotification called."
  if not . isPOST . requestMethod $ req
    then do
      print @String "Notification is not POST"
      respondWith methodNotAllowed
    else do
      body <- getRequestBodyChunk req
      print @String "Notification is POST"
      let headers = requestHeaders req
      res <- case lookup "Twitch-Eventsub-Message-Signature" headers of
        Just sig -> do
          verifyFromTwitch req body secret sig
        _otherwise -> do
          return False
      if not res
        then do
          print @String "Invalid Twitch-Signature in message: "
          print body
          respondWith badRequest
        else case lookup "Twitch-Eventsub-Message-Type" $ requestHeaders req of
          Just messageType -> handleMessageType body tevChan messageType req >>= respondWith
          _otherwise -> do
            print @String "Received Non-Eventsub message:"
            print _otherwise
            respondWith badRequest

verifyFromTwitch :: Request -> ByteString -> ByteString -> ByteString -> IO Bool
verifyFromTwitch req body secret twitchSig = do
  let headers = requestHeaders req
      id = lookup "Twitch-Eventsub-Message-Id" headers
      timestamp = lookup "Twitch-Eventsub-Message-Timestamp" headers
  let hmacMsg = concat3 <$> id <*> timestamp <*> Just body
      hmacHex = createHmacHex secret <$> hmacMsg
      isValid = verifySignature twitchSig <$> hmacHex
  print $ "Twitch-Sig: " <> twitchSig
  print $ "My-Sig: " <> show hmacHex
  print $ "isValid: " <> show isValid
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

handleMessageType :: ByteString -> Chan Twitch.EventNotification -> ByteString -> Request -> IO Response
handleMessageType body _ "webhook_callback_verification" _req = do
  print @String "CallbackVerification challenge received"
  handleCallbackVerification body
handleMessageType body tevChan "notification" _req = do
  print @String "NotificationEvent received"
  handleNotification tevChan body
handleMessageType _body _ "revocation" req = do
  print @String "RevocationEvent received"
  handleRevocation req
handleMessageType _body _ othertype _req = do
  print $ "Received othertype" <> othertype
  return okNoContent

handleCallbackVerification :: ByteString -> IO Response
handleCallbackVerification body = do
  let mChallenge = decodeStrict @Twitch.ChallengeNotification body
  case mChallenge of
    Nothing -> print @String "Unable to decode challenge body"
    Just chall -> print . encode $ chall
  return $ maybe badRequestBody respondWithChallenge mChallenge
  where
    respondWithChallenge mChallenge =
      responseBuilder status200 [("Content-Type", "text/plain")]
        . encodeUtf8Builder
        . Twitch.challengenotificationChallenge
        $ mChallenge

handleNotification :: Chan Twitch.EventNotification -> ByteString -> IO Response
handleNotification tevChan body = do
  print body
  case decodeStrict @Twitch.EventNotification body of
    Nothing -> void $ print @String "unable to decode Twitch.EventNotification"
    Just ev -> writeChan tevChan ev
  -- Always returning okNoContent, since we cannot ask twitch to resend this
  -- message if ill-formatted.
  return okNoContent

-- TODO: Resubscribe to twitch events of interest here.
handleRevocation :: Request -> IO Response
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
