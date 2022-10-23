{-# LANGUAGE DataKinds #-}
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

newtype Horture (l :: LoggingTarget) a = Horture
  { unHorture :: ExceptT HortureError (StateT HortureState (ReaderT HortureStatic IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState HortureState (Horture l) where
  get = Horture get
  {-# INLINEABLE get #-}
  put s = Horture $ put s
  {-# INLINEABLE put #-}

instance MonadReader HortureStatic (Horture l) where
  ask = Horture ask
  {-# INLINEABLE ask #-}
  local mod (Horture h) = Horture $ local mod h
  {-# INLINEABLE local #-}

instance MonadError HortureError (Horture l) where
  throwError err = Horture $ throwError err
  {-# INLINEABLE throwError #-}
  catchError (Horture act) h = Horture $ catchError act (unHorture . h)
  {-# INLINEABLE catchError #-}

runHorture :: HortureState -> HortureStatic -> Horture l a -> IO (Either HortureError a)
runHorture ss rs = flip runReaderT rs . flip evalStateT ss . runExceptT . unHorture
