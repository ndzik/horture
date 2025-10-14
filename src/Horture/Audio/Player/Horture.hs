{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Horture.Audio.Player.Horture where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Lens
import Control.Monad (void, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader
import Foreign (nullPtr)
import Horture.Audio.MacOS
import Horture.Audio.Player.Effects
import Horture.Audio.Player.Player
import Horture.Error (HortureError (AudioSinkInitializationErr))
import Horture.Horture
import Horture.Logging
import Horture.State
import UnliftIO.Exception (bracket)

instance (HortureLogger (Horture m l hdl)) => AudioPlayer (Horture m l hdl) where
  initAudio = initHortureAudio
  deinitAudio = deinitHortureAudio
  clearAudio = clearHortureAudio
  playAudio = playHortureAudio
  withAudio = withHortureAudio

withHortureAudio :: Horture m l hdl a -> Horture m l hdl ()
withHortureAudio action = do
  env <- ask
  let acquire = runHorture env initHortureAudio
      runA _ = runHorture env action
      release _ = runHorture env clearHortureAudio
  void . liftIO $ bracket acquire release runA

initHortureAudio :: Horture m l hdl ()
initHortureAudio = do
  ctx <- liftIO nativeInitPlayer
  when (apHandle ctx == nullPtr) $
    throwError AudioSinkInitializationErr
  asks (^. audioState) >>= liftIO . atomically . flip writeTVar ctx

deinitHortureAudio :: Horture m l hdl ()
deinitHortureAudio = do
  m <- asks _audioState >>= liftIO . readTVarIO
  liftIO $ nativeDeinitPlayer m

clearHortureAudio :: Horture m l hdl ()
clearHortureAudio = do
  m <- asks _audioState >>= liftIO . readTVarIO
  liftIO $ nativeClearAudio m

playHortureAudio :: Sound StaticSoundEffect -> Horture m l hdl ()
playHortureAudio sound = do
  m <- asks _audioState >>= liftIO . readTVarIO
  liftIO $ nativePlayAudio m sound
