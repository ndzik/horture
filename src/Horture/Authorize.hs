{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Follows the credential flows given at:
-- https://dev.twitch.tv/docs/authentication/getting-tokens-oauth/#client-credentials-grant-flow
module Horture.Authorize (authorize, retrieveUserAccessToken) where

import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Char8 (drop)
import Data.Functor (void, (<&>))
import Data.Text (Text, pack)
import Data.Word (Word8)
import Network.HTTP.Types (status200, status400)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Random.Stateful (globalStdGen, uniformListM)
import Text.RawString.QQ
import Twitch.Rest.Authorization
  ( AccessTokenScopes (AccessTokenScopes),
    AuthorizationErrorResponse (..),
    AuthorizationRequest (..),
    AuthorizationResponse (..),
    AuthorizationResponseType (Token),
  )
import Web.FormUrlEncoded (urlDecodeAsForm, urlEncodeAsForm)
import Prelude hiding (drop)

authorize :: FilePath -> IO ()
authorize _pathToConfig = return ()

-- | retrieveAppAccessToken follows the client-credentials-grant-flow, where a
-- client is able to authorize this application by granting certain scope
-- accesses.
--
-- `https://id.twitch.tv/oauth2/token` POST request with URL encoded body:
-- client_id=<app-client-id>
-- &client_secret=<app-client-secret>
-- &grant_type=client_credentials
--
-- Upon success, returns an `app-access-token`:
-- {
--   "access_token": "jostpf5q0uzmxmkba9iyug38kjtgh",
--   "expires_in": 5011271,
--   "token_type": "bearer"
-- }
-- retrieveAppAccessToken :: IO ()
-- retrieveAppAccessToken = return ()

-- | retrieveUserAccessToken follows the implicit client-credentials flow,
-- where the user is directed to `https://id.twitch.tv/oauth2/authorize` and
-- redirected to `http://localhost:3000`. The fragment part of the redirection
-- (everything after `#`) contains the authorization response.
retrieveUserAccessToken :: IO ()
retrieveUserAccessToken = do
  print @String
    "Please open the following authorization link in your browser to give horture access to interact with your account"
  xsrfState <- uniformListM 32 globalStdGen <&> pack . concatMap (show @Word8)
  let ar =
        AuthorizationRequest
          { authorizationrequestClientId = "skkzs4x5fdhpjgfq2w8vfbgf691y8j",
            authorizationrequestForceVerify = Nothing,
            authorizationrequestRedirectUri = "http://localhost:3000",
            authorizationrequestScope = AccessTokenScopes ["channel:manage:redemptions"],
            authorizationrequestResponseType = Token,
            authorizationrequestState = Just xsrfState
          }
  print $ "https://id.twitch.tv/oauth2/authorize?" <> urlEncodeAsForm ar
  runSettings defaultSettings (userAccessTokenApp xsrfState)

-- | userAccessTokenApp is a simple server waiting for a single redirection to
-- happen via the twitch API upon user authorization. Shuts down afterwards.
userAccessTokenApp :: Text -> Application
userAccessTokenApp xsrfState req respondWith = do
  case pathInfo req of
    [] -> do
      case tryDecodeQueryString . rawQueryString $ req of
        Right err -> print err >> respondWith (responseBuilder status200 [] informUserDeniedHTML)
        Left ttt -> print ttt >> respondWith (responseBuilder status200 [] grabFragmentPortionHTML)
    ["authorization"] -> do
      eitherAR <- getRequestBodyChunk req <&> urlDecodeAsForm @AuthorizationResponse . toLazyByteString . byteString
      case eitherAR of
        Right ar
          | authorizationresponseState ar == Just xsrfState -> void . print . authorizationresponseAccessToken $ ar
          | otherwise -> print @String "XSRF detected, not using twitch response. Try again."
        Left err -> print $ "authorization failed: " <> err
      respondWith $ responseBuilder status200 [] "All done, you can proceed in the client"
    _otherwise -> respondWith $ responseBuilder status400 [] "something went wrong"
  where
    tryDecodeQueryString "" = Left "empty query string"
    tryDecodeQueryString qs = urlDecodeAsForm @AuthorizationErrorResponse . toLazyByteString . byteString . drop 1 $ qs

grabFragmentPortionHTML :: Builder
grabFragmentPortionHTML =
  [r|
<html>
  <script>
    // This script will only be run after a previous error check making sure
    // `document.location.hash` should not be empty.
    // `substr(1)` to remove preceeding `#` sign.
    const formBody = document.location.hash.substr(1);
    fetch(
        "http://localhost:3000/authorization",
        {
            method: 'POST',
            body: formBody,
            headers: {
               'Content-Type': 'application/x-www-form-urlencoded'
            }
        }
    );
  </script>
  <h1>
    Forwarded authorization token to client, check your CLI for more information.
  </h1>
</html>
  |]

informUserDeniedHTML :: Builder
informUserDeniedHTML =
  [r|
<html>
  <h1>User Denied Authorization, aborting...</h1>
</html>
  |]
