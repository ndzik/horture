{-# LANGUAGE LambdaCase #-}

module Horture.Server.Server (runHortureServer) where

import Control.Concurrent.Chan.Synchronous
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.Aeson
import Data.ByteArray.Encoding
import Data.ByteString (ByteString, concat)
import Data.Maybe
import Data.Text.Encoding
import Horture.Server.Config
import Horture.Server.Websocket
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Random.Stateful
import qualified Twitch.EventSub.Event as Twitch
import qualified Twitch.EventSub.Notification as Twitch
import Prelude hiding (concat)
import Network.Wai.Handler.WarpTLS

-- | runHortureServer runs the horture server which is exposing the
-- <your-domain>/eventsub endpoint. This can only be used with the production
-- twitch api if served in conjunction with a SSL termination proxy from port
-- 443 to your local port.
runHortureServer :: HortureServerConfig -> IO ()
runHortureServer conf = do
  secret <- uniformByteStringM 64 globalStdGen
  tevChan <- newChan @Twitch.Event
  case (_keyFile conf, _certFile conf) of
    (Just kf, Just cf) -> runTLS (tlsSettings cf kf) defaultSettings . hortureApp conf tevChan $ secret
    _otherwise -> run (_port conf) . hortureApp conf tevChan $ secret

hortureApp :: HortureServerConfig -> Chan Twitch.Event -> ByteString -> Application
hortureApp _conf tevChan secret req respondWith = do
  case pathInfo req of
    -- TODO: Split the singleton `eventsub` endpoint up, s.t. it is
    -- dynamically created after a user has authorized via a websocket
    -- connection. We will use the singleton for now, since this is a MVP.
    ["eventsub"] -> handleTwitchNotification tevChan secret req respondWith
    ["ws"] -> handleWebsocketConn tevChan (_appToken _conf) req respondWith
    _otherwise -> respondWith notFound

handleTwitchNotification :: Chan Twitch.Event -> ByteString -> Application
handleTwitchNotification tevChan secret req respondWith =
  if not . isPOST . requestMethod $ req
    then respondWith methodNotAllowed
    else do
      let headers = requestHeaders req
      res <- case lookup "Twitch-Eventsub-Message-Signature" headers of
        Just sig -> verifyFromTwitch req secret sig
        _otherwise -> return False
      if not res
        then respondWith badRequest
        else case requestHeaders req of
          [("Twitch-Eventsub-Message-Type", messageType)] -> handleMessageType tevChan messageType req >>= respondWith
          _otherwise -> respondWith badRequest

verifyFromTwitch :: Request -> ByteString -> ByteString -> IO Bool
verifyFromTwitch req secret twitchSig = do
  let headers = requestHeaders req
      id = lookup "Twitch-Eventsub-Message-Id" headers
      timestamp = lookup "Twitch-Eventsub-Message-Timestamp" headers
  body <- getRequestBodyChunk req
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

handleMessageType :: Chan Twitch.Event -> ByteString -> Request -> IO Response
handleMessageType _ "webhook_callback_verification" req = handleCallbackVerification req
handleMessageType tevChan "notification" req = handleNotification tevChan req
handleMessageType _ "revocation" req = handleRevocation req
handleMessageType _ _ _req = return okNoContent

handleCallbackVerification :: Request -> IO Response
handleCallbackVerification req = do
  mChallenge <- getRequestBodyChunk req >>= return . decodeStrict @Twitch.ChallengeNotification
  return $ maybe badRequestBody respondWithChallenge mChallenge
  where
    respondWithChallenge mChallenge =
        responseBuilder status200 [("Content-Type", "text/plain")]
        . encodeUtf8Builder
        . Twitch.challengenotificationChallenge
        $ mChallenge

handleNotification :: Chan Twitch.Event -> Request -> IO Response
handleNotification tevChan req = do
  getRequestBodyChunk req
    >>= ( \case
            Nothing -> return ()
            Just (Twitch.EventNotification _ ev) -> writeChan tevChan ev
        )
      . (decodeStrict @Twitch.EventNotification)
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
