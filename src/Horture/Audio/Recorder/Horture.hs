{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Horture.Audio.Recorder.Horture
  (
  )
where

import Horture.Audio.Recorder
import Horture.Horture
import Horture.Logging

#if defined(HORTURE_DARWIN)
import Horture.Audio.MacOS
#endif

import Control.Lens
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State
import Horture.State
import UnliftIO (bracket)

instance (HortureLogger (Horture l hdl)) => AudioRecorder (Horture l hdl) where
  startRecording = do
    ctx <- liftIO $ nativeInitRecorder 0 -- 0 For systemwide audio
    modify $ \st -> st & audioRecording ?~ ctx
    logInfo "Started audio recording"
    liftIO $ nativeStartRecording ctx

  stopRecording = do
    mRec <- gets (^. audioRecording)
    logInfo "Stopping audio recording"
    case mRec of
      Just rec -> liftIO $ nativeStopRecording rec
      Nothing -> return ()
    modify $ \st -> st & audioRecording .~ Nothing

  currentFFTPeak = do
    mRec <- gets (^. audioRecording)
    case mRec of
      Just rec -> liftIO $ nativeCurrentFFT rec
      Nothing -> return (0, 0, 0)

  withRecording = withHortureRecording

withHortureRecording :: forall hdl l a. (HortureLogger (Horture l hdl)) => Horture l hdl a -> Horture l hdl ()
withHortureRecording action = do
  s0 <- get
  env <- ask
  logInfo "Setting up audio recording context"
  let acquire = evalHorture s0 env (startRecording @(Horture l hdl))
      runA (_, s1) = runHorture s1 env action
      release (_, s1) = runHorture s1 env (stopRecording @(Horture l hdl))
  void . liftIO $ bracket acquire release runA
