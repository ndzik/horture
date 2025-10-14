{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Horture.Horture
  ( Horture (..),
    runHorture,
    LoggingTarget (..),
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Horture.Error
import Horture.State

data LoggingTarget = Channel | NoLog deriving (Show)

newtype Horture (l :: LoggingTarget) hdl a = Horture
  { unHorture :: ExceptT HortureError ((ReaderT (HortureEnv hdl) IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader (HortureEnv hdl) (Horture l hdl) where
  ask = Horture ask
  {-# INLINEABLE ask #-}
  local mod (Horture h) = Horture $ local mod h
  {-# INLINEABLE local #-}

instance MonadError HortureError (Horture l hdl) where
  throwError err = Horture $ throwError err
  {-# INLINEABLE throwError #-}
  catchError (Horture act) h = Horture $ catchError act (unHorture . h)
  {-# INLINEABLE catchError #-}

runHorture :: HortureEnv hdl -> Horture l hdl a -> IO (Either HortureError a)
runHorture rs = flip runReaderT rs . runExceptT . unHorture
