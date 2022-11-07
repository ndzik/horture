{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}

module Twitch.Rest.Token
  ( TokenResponse (..),
    RefreshToken (..),
    RefreshTokenRequest (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (urlDecode, urlEncode)
import Web.FormUrlEncoded hiding (fieldLabelModifier)
import Web.HttpApiData

-- | RefreshToken ensuring that refresh tokens are always send in a compatible
-- way between client <-> server.
newtype RefreshToken = RefreshToken Text deriving (Show)

urlEncodeRefreshToken :: Text -> Text
urlEncodeRefreshToken = decodeUtf8 . urlEncode False . encodeUtf8

urlDecodeRefreshToken :: Text -> Text
urlDecodeRefreshToken = decodeUtf8 . urlDecode False . encodeUtf8

instance ToJSON RefreshToken where
  toJSON (RefreshToken t) = String . urlEncodeRefreshToken $ t

instance FromJSON RefreshToken where
  parseJSON = withText "RefreshToken" $ \t -> return . RefreshToken . urlDecodeRefreshToken $ t

instance FromHttpApiData RefreshToken where
  parseUrlPiece = Right . RefreshToken . urlDecodeRefreshToken

instance ToHttpApiData RefreshToken where
  toQueryParam (RefreshToken t) = urlEncodeRefreshToken t

data TokenResponse = TokenResponse
  { accessToken :: !Text,
    refreshToken :: !RefreshToken,
    expiresIn :: !(Maybe Int),
    scope :: ![Text],
    tokenType :: !Text
  }
  deriving (Show)

instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \o -> do
    let scope =
          (.:) @Value o "scope" >>= \case
            String sc -> return [sc]
            Array _ -> (.:) @[Text] o "scope"
            _otherwise -> parseFail "parsing TokenResponse: no scopes given"
    TokenResponse <$> o .: "access_token" <*> o .: "refresh_token" <*> o .:? "expires_in" <*> scope <*> o .: "token_type"

data RefreshTokenRequest = RefreshTokenRequest
  { clientId :: !Text,
    clientSecret :: !Text,
    refreshToken :: !RefreshToken
  }
  deriving (Show)

instance ToJSON RefreshTokenRequest where
  toJSON (RefreshTokenRequest cid cs rt) =
    object
      ["client_id" .= cid, "client_secret" .= cs, "refresh_token" .= rt]

instance FromJSON RefreshTokenRequest where
  parseJSON = withObject "RefreshTokenRequest" $ \o -> do
    (.:) @Text o "grant_type" >>= \case
      "refresh_token" -> RefreshTokenRequest <$> o .: "client_id" <*> o .: "client_secret" <*> o .: "refresh_token"
      otherTokenType -> parseFail $ "parsing RefreshTokenRequest with invalid grant_type: " <> unpack otherTokenType

instance FromForm RefreshTokenRequest where
  fromForm f =
    RefreshTokenRequest
      <$> parseUnique "client_id" f
      <*> parseUnique "client_secret" f
      <*> parseUnique "refresh_token" f

instance ToForm RefreshTokenRequest where
  toForm (RefreshTokenRequest cid cs rt) =
    [ ("grant_type", "refresh_token"),
      ("client_id", cid),
      ("client_secret", cs),
      ("refresh_token", toQueryParam rt)
    ]

$(deriveToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''TokenResponse)
