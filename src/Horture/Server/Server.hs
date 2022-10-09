module Horture.Server.Server (runHortureServer) where

import Control.Monad (guard)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.ByteArray.Encoding
import Data.ByteString (ByteString, concat)
import Data.Maybe
import Horture.Server.Config
import Horture.Server.Websocket
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Random.Stateful
import Prelude hiding (concat)

-- | runHortureServer runs the horture server which is exposing the
-- <your-domain>/eventsub endpoint. This can only be used with the production
-- twitch api if served in conjunction with a SSL termination proxy from port
-- 443 to your local port.
runHortureServer :: HortureServerConfig -> IO ()
runHortureServer conf = do
  secret <- uniformByteStringM 64 globalStdGen
  run (_port conf) . hortureApp conf $ secret

hortureApp :: HortureServerConfig -> ByteString -> Application
hortureApp _conf secret req respondWith = do
  case pathInfo req of
    ["eventsub"] -> handleTwitchNotification secret req respondWith
    ["ws"] -> handleWebsocketConn req respondWith
    _otherwise -> respondWith notFound

handleTwitchNotification :: ByteString -> Application
handleTwitchNotification secret req respondWith =
  if not . isPOST . requestMethod $ req
    then respondWith methodNotAllowed
    else do
      let headers = requestHeaders req
      -- TODO: Throws `user error: mzero`, how to properly handle early abort
      -- without breaking middleware?
      guard =<< case lookup "Twitch-Eventsub-Message-Signature" headers of
        Just sig -> verifyFromTwitch req secret sig
        _otherwise -> return False
      case requestHeaders req of
        [("Twitch-Eventsub-Message-Type", messageType)] -> handleMessageType messageType req >>= respondWith
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

handleMessageType :: ByteString -> Request -> IO Response
handleMessageType "webhook_callback_verification" req = handleCallbackVerification req
handleMessageType "notification" req = handleNotification req
handleMessageType "revocation" req = handleRevocation req
handleMessageType _ _req = return okNoContent

handleCallbackVerification :: Request -> IO Response
handleCallbackVerification _req = do return okNoContent

--  verification <- getRequestBodyChunk req >>= return . decodeStrict @Verification
--  maybe badRequestBody respondWithChallenge verification
--  where
--    respondWithChallenge verification =
--      return
--        . responseBuilder status200 [("Content-Type", "text/plain")]
--        . encodeUtf8Builder
--        . verificationChallenge
--        $ verification

-- TODO: Handle incoming events and push over websocket if available.
handleNotification :: Request -> IO Response
handleNotification _req = return okNoContent

-- TODO: Resubscribe to twitch events of interest here.
handleRevocation :: Request -> IO Response
handleRevocation _req = return okNoContent

okNoContent :: Response
okNoContent = responseBuilder status204 [] mempty

notFound :: Response
notFound = responseBuilder status404 [("Content-Type", "text/plain")] mempty

badRequest :: Response
badRequest = responseBuilder status400 [("Content-Type", "text/plain")] mempty

-- badRequestBody :: Response
-- badRequestBody = responseBuilder status422 [("Content-Type", "text/plain")] mempty

methodNotAllowed :: Response
methodNotAllowed = responseBuilder status405 [("Content-Type", "text/plain")] mempty

isPOST :: ByteString -> Bool
isPOST "POST" = True
isPOST _ = False
