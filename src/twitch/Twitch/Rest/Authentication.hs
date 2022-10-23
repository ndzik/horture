{-# LANGUAGE OverloadedLists #-}

module Twitch.Rest.Authentication
  ( ClientCredentialRequest (..),
    ClientCredentialResponse (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Web.FormUrlEncoded hiding (fieldLabelModifier)
import Web.HttpApiData
import Prelude hiding (unwords, words)

data ClientCredentialRequest = ClientCredentialRequest
  { clientId :: !Text,
    clientSecret :: !Text
  }
  deriving (Show)

instance FromForm ClientCredentialRequest where
  fromForm f =
    ClientCredentialRequest
      <$> parseUnique "client_id" f
      <*> parseUnique "client_secret" f

instance ToForm ClientCredentialRequest where
  toForm ccr =
    [ ("client_id", toQueryParam (clientId ccr)),
      ("client_secret", toQueryParam (clientSecret ccr)),
      ("grant_type", "client_credentials")
    ]

data ClientCredentialResponse = ClientCredentialResponse
  { accessToken :: !Text,
    expiresIn :: !Int,
    tokenType :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''ClientCredentialResponse)
