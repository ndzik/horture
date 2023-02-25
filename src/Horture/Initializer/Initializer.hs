{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Horture.Initializer.Initializer
  ( HortureInitializer (..),
    runHortureInitializer,
  )
where

import Colog
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Horture.Error
import Horture.Horture
import Horture.Initializer.Environment
import Horture.Logging

runHortureInitializer :: HortureInitializerEnvironment -> HortureInitializer l hdl a -> IO (Either HortureError a)
runHortureInitializer env = runExceptT . flip runReaderT env . unHortureInitializer

newtype HortureInitializer (l :: LoggingTarget) hdl a = HortureInitializer
  { unHortureInitializer :: ReaderT HortureInitializerEnvironment (ExceptT HortureError IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError HortureError,
      MonadReader HortureInitializerEnvironment
    )

instance HortureLogger (HortureInitializer 'Channel hdl) where
  logEvent ev = asks (Just . (^. logChan)) >>= (\la -> withColog Info la (logEvent' ev)) . logChannel
  logInfo msg = asks (Just . (^. logChan)) >>= (\la -> withColog Info la msg) . logChannel
  logError msg = asks (Just . (^. logChan)) >>= (\la -> withColog Error la msg) . logChannel
  logWarn msg = asks (Just . (^. logChan)) >>= (\la -> withColog Warning la msg) . logChannel

instance HortureLogger (HortureInitializer 'NoLog hdl) where
  logEvent _ = return ()
  logInfo _ = return ()
  logError _ = return ()
  logWarn _ = return ()
