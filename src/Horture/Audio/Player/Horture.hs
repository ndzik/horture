{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Horture.Audio.Player.Horture where

import Control.Lens
import Control.Monad (void, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader
import Control.Monad.State
import Foreign (nullPtr)
import Horture.Audio.MacOS
import Horture.Audio.Player.Effects
import Horture.Audio.Player.Player
import Horture.Error (HortureError (AudioSinkInitializationErr))
import Horture.Horture
import Horture.Logging
import Horture.State
import UnliftIO.Exception (bracket)

instance (HortureLogger (Horture l hdl)) => AudioPlayer (Horture l hdl) where
  initAudio = initHortureAudio
  deinitAudio = deinitHortureAudio
  clearAudio = clearHortureAudio
  playAudio = playHortureAudio
  withAudio = withHortureAudio

withHortureAudio :: Horture l hdl a -> Horture l hdl ()
withHortureAudio action = do
  s0 <- get
  env <- ask
  let acquire = evalHorture s0 env initHortureAudio
      runA (_, s1) = runHorture s1 env action
      release (_, s1) = runHorture s1 env clearHortureAudio
  void $ liftIO $ bracket acquire release runA

initHortureAudio :: Horture l hdl ()
initHortureAudio = do
  ctx <- liftIO nativeInitPlayer
  when (apHandle ctx == nullPtr) $
    throwError AudioSinkInitializationErr
  modify $ \st -> st & audioState .~ ctx

deinitHortureAudio :: Horture l hdl ()
deinitHortureAudio = do
  m <- gets _audioState
  liftIO $ nativeDeinitPlayer m

clearHortureAudio :: Horture l hdl ()
clearHortureAudio = do
  m <- gets _audioState
  liftIO $ nativeClearAudio m

playHortureAudio :: Sound StaticSoundEffect -> Horture l hdl ()
playHortureAudio sound = do
  m <- gets _audioState
  liftIO $ print $ "Playing sound: " ++ show sound
  liftIO $ nativePlayAudio m sound
