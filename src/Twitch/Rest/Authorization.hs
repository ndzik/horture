{-# LANGUAGE OverloadedLists #-}

module Twitch.Rest.Authorization
  ( AuthorizationCodeRequest (..),
    AuthorizationResponseType (..),
    AuthorizationRequest (..),
    AccessTokenCodeType (..),
  )
where

import Data.Text (Text, unwords, words)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Exts
import Web.FormUrlEncoded hiding (fieldLabelModifier)
import Web.HttpApiData
import Prelude hiding (unwords, words)
import Network.HTTP.Types (urlDecode)

newtype AccessTokenCodeType = AccessTokenCodeType [Text] deriving (Show)

instance ToHttpApiData AccessTokenCodeType where
  toQueryParam (AccessTokenCodeType ss) = toQueryParam . unwords $ ss

instance FromHttpApiData AccessTokenCodeType where
  parseUrlPiece = Right . AccessTokenCodeType . words . decodeUtf8 . urlDecode True . encodeUtf8

data AuthorizationCodeRequest = AuthorizationCodeRequest
  { authorizationcoderequesetClientId :: !Text,
    authorizationcoderequesetClientSecret :: !Text,
    authorizationcoderequesetCode :: !Text,
    authorizationcoderequesetRedirectUri :: !Text
  }
  deriving (Show)

instance FromForm AuthorizationCodeRequest where
  fromForm f =
    AuthorizationCodeRequest
      <$> parseUnique "client_id" f
      <*> parseUnique "client_secret" f
      <*> parseUnique "code" f
      <*> parseUnique "refresh_token" f

instance ToForm AuthorizationCodeRequest where
  toForm (AuthorizationCodeRequest cid cs cod uri) =
    [ ("grant_type", "authorization_code"),
      ("client_id", cid),
      ("client_secret", cs),
      ("code", toQueryParam cod),
      ("refresh_token", toQueryParam uri)
    ]

data AuthorizationResponseType = Token | Code deriving (Show)

instance ToHttpApiData AuthorizationResponseType where
  toQueryParam Token = "token"
  toQueryParam Code = "code"

instance FromHttpApiData AuthorizationResponseType where
  parseUrlPiece "token" = Right Token
  parseUrlPiece "code" = Right Code
  parseUrlPiece other = Left $ "unknown autorization code type encountered: " <> other

data AuthorizationResponse = AuthorizationResponse
  { authorizationresponseCode :: !Text,
    authorizationresponseScope :: !AccessTokenCodeType,
    authorizationresponseState :: !Text
  }
  deriving (Show)

instance FromForm AuthorizationResponse where
  fromForm f =
    AuthorizationResponse
      <$> parseUnique "code" f
      <*> parseUnique "scope" f
      <*> parseUnique "state" f

instance ToForm AuthorizationResponse where
  toForm acr =
    [ ("code", toQueryParam . authorizationresponseCode $ acr),
      ("scope", toQueryParam . authorizationresponseScope $ acr),
      ("state", toQueryParam . authorizationresponseState $ acr)
    ]

data AuthorizationRequest = AuthorizationRequest
  { authorizationrequestClientId :: !Text,
    authorizationrequestForceVerify :: !(Maybe Bool),
    authorizationrequestRedirectUri :: !Text,
    authorizationrequestScope :: !AccessTokenCodeType,
    authorizationrequestResponseType :: !AuthorizationResponseType,
    authorizationrequestState :: !(Maybe Text)
  }
  deriving (Show)

instance FromForm AuthorizationRequest where
  fromForm f =
    AuthorizationRequest
      <$> parseUnique "client_id" f
      <*> parseMaybe "force_verify" f
      <*> parseUnique "redirect_uri" f
      <*> parseUnique "scope" f
      <*> parseUnique "code" f
      <*> parseMaybe "state" f

instance ToForm AuthorizationRequest where
  toForm acr =
    fromList
      . prependIfJust "force_verify" (authorizationrequestForceVerify acr)
      . prependIfJust "state" (authorizationrequestState acr)
      $ [ ("response_type", toQueryParam . authorizationrequestResponseType $ acr),
          ("client_id", toQueryParam . authorizationrequestClientId $ acr),
          ("redirect_uri", toQueryParam . authorizationrequestRedirectUri $ acr),
          ("scope", toQueryParam . authorizationrequestScope $ acr)
        ]
    where
      prependIfJust :: (ToHttpApiData a) => Text -> Maybe a -> [Item Form] -> [Item Form]
      prependIfJust label (Just v) rs = (label, toQueryParam v) : rs
      prependIfJust _ Nothing rs = rs
