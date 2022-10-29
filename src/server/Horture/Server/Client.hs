{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Horture.Server.Client where

import Control.Concurrent.Chan.Synchronous
import Control.Exception (bracket)
import Control.Lens
import Control.Monad.RWS
import Data.ByteString (ByteString)
import Data.Default
import Data.Text (Text)
import Katip
import Network.WebSockets hiding (Request, Response, requestHeaders)
import Servant.Client
import System.IO (stdout)
import qualified Twitch.EventSub.Notification as Twitch
import Twitch.Rest
import Prelude hiding (concat)

runHortureClientWS ::
  HortureClient () ->
  ByteString ->
  Text ->
  Text ->
  BaseUrl ->
  Chan Twitch.EventNotification ->
  AuthorizationToken ->
  ServerApp
runHortureClientWS cl secret cid cb endpoint tevChan aat pendingConn = do
  conn <- acceptRequest pendingConn
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V3
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "horture-server" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    let conf =
          HortureClientConfig
            { _conn = conn,
              _secret = secret,
              _twitchApiEndpoint = endpoint,
              _twitchEventQueue = tevChan,
              _twitchApiCallback = cb,
              _logEnv = le,
              _logContext = mempty,
              _logNamespace = mempty,
              _appClientId = cid,
              _appAccess = aat
            }
    evalRWST (unHortureClient cl) conf def <&> fst

instance Default HortureClientState where
  def =
    HortureClientState
      { _hcsuserAccessToken = Nothing,
        _hcsappAccessToken = Nothing
      }

newtype HortureClient a = HortureClient
  { unHortureClient :: RWST HortureClientConfig () HortureClientState IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader HortureClientConfig,
      MonadState HortureClientState,
      MonadWriter (),
      MonadFail
    )

data HortureClientConfig = HortureClientConfig
  { _conn :: !Connection,
    _secret :: !ByteString,
    _twitchApiEndpoint :: !BaseUrl,
    _twitchApiCallback :: !Text,
    _twitchEventQueue :: !(Chan Twitch.EventNotification),
    _appClientId :: !Text,
    _logEnv :: !LogEnv,
    _logContext :: !LogContexts,
    _logNamespace :: !Namespace,
    _appAccess :: !AuthorizationToken
  }

data HortureClientState = HortureClientState
  { _hcsuserAccessToken :: !(Maybe Text),
    _hcsappAccessToken :: !(Maybe Text)
  }
  deriving (Show)

makeLenses ''HortureClientConfig
makeLenses ''HortureClientState

instance Katip HortureClient where
  getLogEnv = view logEnv
  localLogEnv f (HortureClient m) = HortureClient $ local (over logEnv f) m

instance KatipContext HortureClient where
  getKatipContext = view logContext
  localKatipContext f (HortureClient m) = HortureClient $ local (over logContext f) m
  getKatipNamespace = view logNamespace
  localKatipNamespace f (HortureClient m) = HortureClient $ local (over logNamespace f) m
