{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Horture.Horture
  ( Horture (..),
    runHorture,
    LoggingTarget (..),
    Mode (..),
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Text as T
import Horture.Debug
import Horture.Error
import Horture.State
import qualified System.Clock as Clock

data LoggingTarget = Channel | NoLog deriving (Show)

data Mode = Prod | Dev deriving (Show)

newtype Horture (m :: Mode) (l :: LoggingTarget) hdl a = Horture
  { unHorture :: ExceptT HortureError ((ReaderT (HortureEnv hdl) IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader (HortureEnv hdl) (Horture m l hdl) where
  ask = Horture ask
  {-# INLINEABLE ask #-}
  local mod (Horture h) = Horture $ local mod h
  {-# INLINEABLE local #-}

instance MonadError HortureError (Horture m l hdl) where
  throwError err = Horture $ throwError err
  {-# INLINEABLE throwError #-}
  catchError (Horture act) h = Horture $ catchError act (unHorture . h)
  {-# INLINEABLE catchError #-}

instance Debuggable (Horture 'Prod hdl a) where
  timeCPU _ action = action
  {-# INLINEABLE timeCPU #-}

instance Debuggable (Horture 'Dev hdl a) where
  timeCPU label action = do
    start <- liftIO $ Clock.getTime Clock.Monotonic
    result <- action
    end <- liftIO $ Clock.getTime Clock.Monotonic
    let ms :: Double = fromIntegral (Clock.toNanoSecs (end - start)) / 1e6
    liftIO . print $ label <> " (ms): " <> (T.pack $ show ms)
    return result
  {-# INLINEABLE timeCPU #-}

runHorture :: HortureEnv hdl -> Horture m l hdl a -> IO (Either HortureError a)
runHorture rs = flip runReaderT rs . runExceptT . unHorture
