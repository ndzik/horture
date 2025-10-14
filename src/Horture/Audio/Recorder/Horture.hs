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
import Horture.State
import UnliftIO (bracket, putMVar, tryReadMVar, tryTakeMVar)

instance (HortureLogger (Horture l hdl)) => AudioRecorder (Horture l hdl) where
  startRecording = do
    ctx <- liftIO $ nativeInitRecorder 0 -- 0 For systemwide audio
    asks (^. audioRecording) >>= liftIO . flip putMVar ctx
    logInfo "Started audio recording"
    liftIO $ nativeStartRecording ctx

  stopRecording = do
    mRec <- asks (^. audioRecording) >>= liftIO . tryTakeMVar
    logInfo "Stopping audio recording"
    case mRec of
      Just rec -> liftIO $ nativeStopRecording rec
      Nothing -> return ()

  currentFFTPeak = do
    mRec <- asks (^. audioRecording) >>= liftIO . tryReadMVar
    case mRec of
      Just rec -> liftIO $ nativeCurrentFFT rec
      Nothing -> return (0, 0, 0)

  withRecording = withHortureRecording

withHortureRecording :: forall hdl l a. (HortureLogger (Horture l hdl)) => Horture l hdl a -> Horture l hdl ()
withHortureRecording action = do
  env <- ask
  logInfo "Setting up audio recording context"
  let acquire = runHorture env (startRecording @(Horture l hdl))
      runA _ = runHorture env action
      release _ = runHorture env (stopRecording @(Horture l hdl))
  void . liftIO $ bracket acquire release runA
