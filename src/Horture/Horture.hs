module Horture.Horture (Horture, runHorture) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Horture.Error
import Horture.State

type Horture a = ExceptT HortureError (StateT HortureState (ReaderT HortureStatic IO)) a

runHorture :: HortureState -> HortureStatic -> Horture a -> IO (Either HortureError a)
runHorture ss rs = flip runReaderT rs . flip evalStateT ss . runExceptT
