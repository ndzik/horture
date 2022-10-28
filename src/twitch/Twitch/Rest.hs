{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.Rest
  ( module Authentication,
    module Authorization,
    module Token,
    twitchTokenClient,
    TwitchTokenClient (..),
    TwitchChannelPointsClient (..),
    AuthorizationToken (..),
    twitchChannelPointsClient,
    TwitchUsersClient (..),
    twitchUsersClient,
    TwitchEventSubClient (..),
    twitchEventSubClient,
  )
where

import Data.Proxy
import Data.Text (Text, words)
import Servant.API
import Servant.Client
import Twitch.EventSub
import Twitch.Rest.Authentication as Authentication
import Twitch.Rest.Authorization as Authorization
import Twitch.Rest.DataResponse
import Twitch.Rest.Token as Token
import Twitch.Rest.Types
import Prelude hiding (words)

newtype TwitchTokenClient = TwitchTokenClient
  { getAppAccessToken :: ClientCredentialRequest -> ClientM ClientCredentialResponse
  }

type TokenApi = GetAppAccessToken

type GetAppAccessToken =
  "token"
    :> ReqBody '[FormUrlEncoded] ClientCredentialRequest
    :> Post '[JSON] ClientCredentialResponse

twitchTokenClient :: TwitchTokenClient
twitchTokenClient =
  let _getAppAccessToken = client (Proxy @TokenApi)
   in TwitchTokenClient
        { getAppAccessToken = _getAppAccessToken
        }

newtype AuthorizationToken = AuthorizationToken Text

instance ToHttpApiData AuthorizationToken where
  toQueryParam (AuthorizationToken t) = "Bearer " <> t

instance FromHttpApiData AuthorizationToken where
  parseUrlPiece t = case words t of
    ["Bearer", token] -> Right . AuthorizationToken $ token
    _otherwise -> Left "invalid authorization token encoding"

data TwitchChannelPointsClient = TwitchChannelPointsClient
  { -- | broadcaster_id -> id -> only_manageable_rewards.
    getCustomRewards ::
      Text ->
      Maybe Text ->
      Maybe Bool ->
      ClientM (DataResponse [GetCustomRewardsData]),
    -- | broadcaster_id -> id.
    deleteCustomReward ::
      Text ->
      Text ->
      ClientM NoContent,
    -- | breadcaster_id -> CreateCustomRewardBody.
    createCustomReward ::
      Text ->
      CreateCustomRewardBody ->
      ClientM (DataResponse [GetCustomRewardsData])
  }

twitchChannelPointsClient :: Text -> AuthorizationToken -> TwitchChannelPointsClient
twitchChannelPointsClient clientid at =
  let _getCustomRewards
        :<|> _deleteCustomReward
        :<|> _createCustomReward = client (Proxy @ChannelPointsApi)
   in TwitchChannelPointsClient
        { getCustomRewards = _getCustomRewards clientid at,
          deleteCustomReward = _deleteCustomReward clientid at,
          createCustomReward = _createCustomReward clientid at
        }

type ChannelPointsApi =
  GetCustomRewards
    :<|> DeleteCustomReward
    :<|> CreateCustomReward

type GetCustomRewards =
  "channel_points"
    :> "custom_rewards"
    :> Header' [Required, Strict] "Client-Id" Text
    :> Header' [Required, Strict] "Authorization" AuthorizationToken
    :> QueryParam' [Required, Strict] "broadcaster_id" Text
    :> QueryParam "id" Text
    :> QueryParam "only_manageable_rewards" Bool
    :> Get '[JSON] (DataResponse [GetCustomRewardsData])

type DeleteCustomReward =
  "channel_points"
    :> "custom_rewards"
    :> Header' [Required, Strict] "Client-Id" Text
    :> Header' [Required, Strict] "Authorization" AuthorizationToken
    :> QueryParam' [Required, Strict] "broadcaster_id" Text
    :> QueryParam' [Required, Strict] "id" Text
    :> DeleteNoContent

type CreateCustomReward =
  "channel_points"
    :> "custom_rewards"
    :> Header' [Required, Strict] "Client-Id" Text
    :> Header' [Required, Strict] "Authorization" AuthorizationToken
    :> QueryParam' [Required, Strict] "broadcaster_id" Text
    :> ReqBody '[JSON] CreateCustomRewardBody
    :> Post '[JSON] (DataResponse [GetCustomRewardsData])

newtype TwitchUsersClient = TwitchUsersClient
  { getUsers ::
      Maybe Text ->
      [Text] ->
      ClientM (DataResponse [GetUserInformation])
  }

twitchUsersClient :: Text -> AuthorizationToken -> TwitchUsersClient
twitchUsersClient clientid at =
  let _getUsers = client (Proxy @UsersApi)
   in TwitchUsersClient
        { getUsers = _getUsers clientid at
        }

type UsersApi = GetUsers

type GetUsers =
  "users"
    :> Header' [Required, Strict] "Client-Id" Text
    :> Header' [Required, Strict] "Authorization" AuthorizationToken
    :> QueryParam "id" Text
    :> QueryParams "login" Text
    :> Get '[JSON] (DataResponse [GetUserInformation])

data TwitchEventSubClient = TwitchEventSubClient
  { eventsubSubscribe :: WebhookRequest -> ClientM SubscriptionResponse,
    eventsubGetSubscriptions :: Maybe Text -> ClientM SubscriptionResponse,
    eventsubDeleteSubscription :: Text -> ClientM NoContent
  }

twitchEventSubClient :: Text -> AuthorizationToken -> TwitchEventSubClient
twitchEventSubClient clientid at =
  let _eventsubSubscribe
        :<|> _eventsubGetSubscriptions
        :<|> _eventsubDeleteSubscription = client (Proxy @EventSubApi)
   in TwitchEventSubClient
        { eventsubSubscribe = _eventsubSubscribe clientid at,
          eventsubGetSubscriptions = _eventsubGetSubscriptions clientid at,
          eventsubDeleteSubscription = _eventsubDeleteSubscription clientid at
        }

type EventSubApi =
  EventSubSubscribe
    :<|> EventSubGetSubscriptions
    :<|> EventSubDeleteSubscriptions

type EventSubSubscribe =
  "eventsub"
    :> "subscriptions"
    :> Header' [Required, Strict] "Client-Id" Text
    :> Header' [Required, Strict] "Authorization" AuthorizationToken
    :> ReqBody '[JSON] WebhookRequest
    :> PostAccepted '[JSON] SubscriptionResponse

type EventSubGetSubscriptions =
  "eventsub"
    :> "subscriptions"
    :> Header' [Required, Strict] "Client-Id" Text
    :> Header' [Required, Strict] "Authorization" AuthorizationToken
    :> QueryParam "status" Text
    :> Get '[JSON] SubscriptionResponse

type EventSubDeleteSubscriptions =
  "eventsub"
    :> "subscriptions"
    :> Header' [Required, Strict] "Client-Id" Text
    :> Header' [Required, Strict] "Authorization" AuthorizationToken
    :> QueryParam' [Required, Strict] "id" Text
    :> DeleteNoContent
