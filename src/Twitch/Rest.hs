{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.Rest
  ( module Authentication,
    module Authorization,
    module Token,
    twitchTokenClient,
    TwitchTokenClient (..),
  )
where

import Data.Proxy
import Servant.API
import Servant.Client
import Twitch.Rest.Authentication as Authentication
import Twitch.Rest.Authorization as Authorization
import Twitch.Rest.Token as Token

newtype TwitchTokenClient = TwitchTokenClient
  { getAppAccessToken :: ClientCredentialRequest -> ClientM ClientCredentialResponse
  }

type TokenApi = GetAppAccessToken

type GetAppAccessToken = "token"
  :> ReqBody '[FormUrlEncoded] ClientCredentialRequest :> Post '[JSON] ClientCredentialResponse

twitchTokenClient :: TwitchTokenClient
twitchTokenClient =
  let _getAppAccessToken = client (Proxy @TokenApi)
   in TwitchTokenClient
        { getAppAccessToken = _getAppAccessToken
        }
