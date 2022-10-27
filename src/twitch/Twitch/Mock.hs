{-# LANGUAGE DataKinds #-}

-- | Twitch.Mock provides a client interface to mock specific endpoints used in
-- twitch's twitch-cli.
module Twitch.Mock
  ( twitchMockAuthClient,
    TwitchMockAuthClient (..),
  )
where

import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client
import Twitch.Rest.Authentication
import Twitch.Rest.Authorization

newtype TwitchMockAuthClient = TwitchMockAuthClient
  { getUserToken :: Text -> Maybe AccessTokenScopes -> ClientM ClientCredentialResponse
  }

twitchMockAuthClient :: Text -> Text -> TwitchMockAuthClient
twitchMockAuthClient clientid secret =
  let _getUserToken = client (Proxy @MockAuthApi)
   in TwitchMockAuthClient
        { getUserToken = _getUserToken clientid secret "user_token"
        }

type MockAuthApi = GetUserToken

type GetUserToken =
  "authorize"
    :> QueryParam' [Required, Strict] "client_id" Text
    :> QueryParam' [Required, Strict] "client_secret" Text
    :> QueryParam' [Required, Strict] "grant_type" Text
    :> QueryParam' [Required, Strict] "user_id" Text
    :> QueryParam "scope" AccessTokenScopes
    :> Post '[JSON] ClientCredentialResponse
