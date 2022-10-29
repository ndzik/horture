{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Horture.Server.Application where

import Control.Concurrent.Chan.Synchronous
import Control.Exception (bracket)
import Control.Lens
import Control.Monad.Reader
import Data.ByteString
import Horture.Server.Config
import Horture.Server.Reader
import Katip
import Network.Wai
import System.IO (stdout)
import qualified Twitch.EventSub.Notification as Twitch

runHortureServer :: HortureHandler' -> HortureServerConfig -> Chan Twitch.EventNotification -> ByteString -> Application
runHortureServer app cfg chan s req respondWith = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "horture-server" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    let env =
          HortureServerEnv
            { _conf = cfg,
              _notificationChan = chan,
              _secret = s,
              _logEnv = le,
              _logContext = mempty,
              _logNamespace = mempty
            }
    runReaderT (unHortureServer $ app req (liftIO . respondWith)) env

newtype HortureServer a = HortureServer
  { unHortureServer :: ReaderT HortureServerEnv IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HortureServerEnv)

type HortureHandler' = Request -> (Response -> IO ResponseReceived) -> HortureServer ResponseReceived

type HortureHandler = Request -> (Response -> HortureServer ResponseReceived) -> HortureServer ResponseReceived

instance Katip HortureServer where
  getLogEnv = view logEnv
  localLogEnv f (HortureServer m) = HortureServer $ local (over logEnv f) m

instance KatipContext HortureServer where
  getKatipContext = view logContext
  localKatipContext f (HortureServer m) = HortureServer $ local (over logContext f) m
  getKatipNamespace = view logNamespace
  localKatipNamespace f (HortureServer m) = HortureServer $ local (over logNamespace f) m
