{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Twitch.Server (runTwitchSubLocal, runTwitchSub) where

import Control.Monad (guard)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.Aeson (decodeStrict)
import Data.ByteArray.Encoding
import Data.ByteString (ByteString, concat)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.WebSockets
import Network.WebSockets hiding (Request, Response, requestHeaders)
import System.Random.Stateful
import Twitch.Types
import Prelude hiding (concat)

-- | runTwitchSub runs the twitch sub server with the given certificate and key
-- on the given port. Can be used in production with officially CA signed SSL
-- certificates or alternatively locally in tests with self-signed
-- certificates.
runTwitchSub :: FilePath -> FilePath -> Port -> IO ()
runTwitchSub cert key port = server (runTLS (tlsSettings cert key) (setPort port defaultSettings))

-- | runTwitchSubLocal runs the server in a local environment. This cannot be
-- used with the production twitch api, because webhooks ONLY work external
-- reachable servers with an officially CA signed SSL certificate.
runTwitchSubLocal :: Port -> IO ()
runTwitchSubLocal port = server (run port)

server :: (Application -> IO ()) -> IO ()
server runApp = do
  secret <- uniformByteStringM 64 globalStdGen
  -- TODO: Initially subscribe to twitch events of interest here.
  runApp . twitchSubApp $ secret

twitchSubApp :: ByteString -> Application
twitchSubApp secret = websocketsOr defaultConnectionOptions hortureWSApp (hortureTwitchApp secret)

-- | hortureWSApp will forward and multiplex received webhook messages to a
-- connected client.
hortureWSApp :: ServerApp
hortureWSApp pendingConn = do
  conn <- acceptRequest pendingConn
  sendTextData @Text conn "Hello, client!"

-- | hortureTwitchApp acts as the webhook for twitch to send subscribed events
-- to.
hortureTwitchApp :: ByteString -> Application
hortureTwitchApp secret req respondWith = do
  response <- case pathInfo req of
    ["eventsub"] -> handleTwitchNotification secret req
    _otherwise -> notFound
  respondWith response

handleTwitchNotification :: ByteString -> Request -> IO Response
handleTwitchNotification secret req =
  if not . isPOST . requestMethod $ req
    then methodNotAllowed
    else do
      let headers = requestHeaders req
      -- TODO: Throws `user error: mzero`, how to properly handle early abort
      -- without breaking middleware?
      guard =<< case lookup "Twitch-Eventsub-Message-Signature" headers of
        Just sig -> verifyFromTwitch req secret sig
        _otherwise -> return False
      case requestHeaders req of
        [("Twitch-Eventsub-Message-Type", messageType)] -> handleMessageType messageType req
        _otherwise -> badRequest

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
handleMessageType _ _req = okNoContent

handleCallbackVerification :: Request -> IO Response
handleCallbackVerification req = do
  verification <- getRequestBodyChunk req >>= return . decodeStrict @(Verification RewardRedemptionCondition)
  maybe badRequestBody respondWithChallenge verification
  where
    respondWithChallenge verification =
      return
        . responseBuilder status200 [("Content-Type", "text/plain")]
        . encodeUtf8Builder
        . verificationChallenge
        $ verification

-- TODO: Handle incoming events and push over websocket if available.
handleNotification :: Request -> IO Response
handleNotification _req = okNoContent

-- TODO: Resubscribe to twitch events of interest here.
handleRevocation :: Request -> IO Response
handleRevocation _req = okNoContent

okNoContent :: IO Response
okNoContent = return $ responseBuilder status204 [] mempty

notFound :: IO Response
notFound = return $ responseBuilder status404 [("Content-Type", "text/plain")] mempty

badRequest :: IO Response
badRequest = return $ responseBuilder status400 [("Content-Type", "text/plain")] mempty

badRequestBody :: IO Response
badRequestBody = return $ responseBuilder status422 [("Content-Type", "text/plain")] mempty

methodNotAllowed :: IO Response
methodNotAllowed = return $ responseBuilder status405 [("Content-Type", "text/plain")] mempty

isPOST :: ByteString -> Bool
isPOST "POST" = True
isPOST _ = False
