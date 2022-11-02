{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Monad.State
import Horture.Error
import Horture.State

data LoggingTarget = Channel | NoLog deriving (Show)

newtype Horture (l :: LoggingTarget) hdl a = Horture
  { unHorture :: ExceptT HortureError (StateT (HortureState hdl) (ReaderT HortureStatic IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState (HortureState hdl) (Horture l hdl) where
  get = Horture get
  {-# INLINEABLE get #-}
  put s = Horture $ put s
  {-# INLINEABLE put #-}

instance MonadReader HortureStatic (Horture l hdl) where
  ask = Horture ask
  {-# INLINEABLE ask #-}
  local mod (Horture h) = Horture $ local mod h
  {-# INLINEABLE local #-}

instance MonadError HortureError (Horture l hdl) where
  throwError err = Horture $ throwError err
  {-# INLINEABLE throwError #-}
  catchError (Horture act) h = Horture $ catchError act (unHorture . h)
  {-# INLINEABLE catchError #-}

runHorture :: HortureState hdl -> HortureStatic -> Horture l hdl a -> IO (Either HortureError a)
runHorture ss rs = flip runReaderT rs . flip evalStateT ss . runExceptT . unHorture
