{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Twitch.Server (runTwitchSub) where

import Control.Monad (guard)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.Aeson (decodeStrict)
import Data.ByteArray.Encoding
import Data.ByteString (ByteString, concat)
import Data.Maybe
import Data.Text.Encoding (encodeUtf8Builder)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Twitch.Types
import Prelude hiding (concat)

runTwitchSub :: Port -> IO ()
runTwitchSub port = run port twitchSubApp

twitchSubApp :: Application
twitchSubApp req respondWith = do
  response <- case pathInfo req of
    ["eventsub"] -> handleTwitchNotification req
    _otherwise -> notFound
  respondWith response

handleTwitchNotification :: Request -> IO Response
handleTwitchNotification req = do
  let headers = requestHeaders req
      secret = "someSecret"
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
handleMessageType _ _req = badRequest

handleCallbackVerification :: Request -> IO Response
handleCallbackVerification req = do
  verification <- getRequestBodyChunk req >>= return . decodeStrict @Verification
  maybe badRequestBody respondWithChallenge verification
  where
    respondWithChallenge verification =
      return
        . responseBuilder status200 [("Content-Type", "text/plain")]
        . encodeUtf8Builder
        . verificationChallenge
        $ verification

handleNotification :: Request -> IO Response
handleNotification _req = okResponse

handleRevocation :: Request -> IO Response
handleRevocation _req = okResponse

okResponse :: IO Response
okResponse = return $ responseBuilder status200 [] mempty

notFound :: IO Response
notFound = return $ responseBuilder status404 [("Content-Type", "text/plain")] mempty

badRequest :: IO Response
badRequest = return $ responseBuilder status400 [("Content-Type", "text/plain")] mempty

badRequestBody :: IO Response
badRequestBody = return $ responseBuilder status422 [("Content-Type", "text/plain")] mempty
