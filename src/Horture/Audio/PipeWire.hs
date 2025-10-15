{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PipeWire backend for Horture to capture desktop audio.
module Horture.Audio.PipeWire
  ( mkAudioCallback,
    runPipeWire,
    onProcessAudio,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Loop
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Array.CArray.Base
import Data.Maybe
import qualified Data.RingBuffer as Ringbuffer
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Horture.Audio.Recorder
import Horture.Horture
import Horture.Logging
import Horture.State
import Math.FFT
import Math.FFT.Base
import UnliftIO.Exception

deriving instance FFTWReal CFloat

type AudioCallback = CUInt -> CUInt -> CUInt -> Ptr CFloat -> IO Bool

instance (HortureLogger (Horture l hdl)) => AudioRecorder (Horture l hdl) where
  startRecording = startHortureRecording
  stopRecording = stopHortureRecording
  currentFFTPeak = currentHortureFFTPeak
  withRecording = withHortureRecording

takeAvg :: Int -> [(Double, Double, Double)] -> Horture l hdl (Double, Double, Double)
takeAvg n ffts = do
  let (b, m, h) = foldr go (0, 0, 0) ffts
      n' = fromIntegral n
  return (b / n', m / n', h / n')
  where
    go (bassA, midA, highA) (bassB, midB, highB) = (bassA + bassB, midA + midB, highA + highB)

foreign import ccall "wrapper"
  mkAudioCallback :: AudioCallback -> IO (FunPtr AudioCallback)

foreign import ccall "record.c run"
  runPipeWire :: FunPtr AudioCallback -> IO ()

currentHortureFFTPeak :: (HortureLogger (Horture l hdl)) => Horture l hdl (Double, Double, Double)
currentHortureFFTPeak =
  gets (^. audioStorage) >>= liftIO . readTVarIO >>= \case
    Nothing -> return (0, 0, 0)
    Just res -> do
      gets (^. mvgAvg) >>= \fftBuf -> do
        liftIO $ Ringbuffer.append res fftBuf
        l <- liftIO $ Ringbuffer.toList fftBuf
        takeAvg (fromIntegral . length $ l) l

stopHortureRecording :: (HortureLogger (Horture l hdl)) => Horture l hdl ()
stopHortureRecording =
  gets (^. audioRecording) >>= \case
    Nothing -> return ()
    Just stopIt -> liftIO (putMVar stopIt ()) >> audioRecording .= Nothing

startHortureRecording :: (HortureLogger (Horture l hdl)) => Horture l hdl ()
startHortureRecording =
  gets (^. audioRecording) >>= \case
    Nothing -> do
      stopIt <- liftIO newEmptyMVar
      storage <- liftIO $ newTVarIO Nothing
      void . liftIO $ mkAudioCallback (onProcessAudio stopIt storage) >>= forkIO . runPipeWire
      audioRecording .= Just stopIt
      audioStorage .= storage
    Just _ -> stopRecording >> startRecording

withHortureRecording :: forall hdl l a. (HortureLogger (Horture l hdl)) => Horture l hdl a -> Horture l hdl ()
withHortureRecording action = do
  s <- get
  env <- ask
  let acquire = evalHorture s env (startRecording @(Horture l hdl))
      action' (_, s') = runHorture s' env action
      release (_, s') = runHorture s' env (stopRecording @(Horture l hdl))
  void . liftIO $ bracket acquire release action'

-- | onProcessAudio is the audio callback passed to pipewire when a new audio
-- sample is ready to be processed. It is parameterized on an MVar signal
-- indicating whether the processing is supposed to stop and a TVar as storage
-- for the audioprocessing result.
onProcessAudio :: MVar () -> TVar (Maybe FFTSnapshot) -> AudioCallback
onProcessAudio shouldStop stm sampleRate _nChannels nSamples samples = do
  cSamples <- CArray (0 :: Int) (fromIntegral $ nSamples - 1) (fromIntegral nSamples) <$> newForeignPtr_ samples
  let fftRes = dst1 cSamples
      nyqFreq = fromIntegral @_ @Double sampleRate / 2
      n = fromIntegral nSamples / nyqFreq
      bassIndex = round $ 4500 * n -- 0 - 4500 Hz
      midIndex = round $ 9000 * n -- 4500 - 9000 Hz
      highIndex = round $ 16000 * n -- 9000 - 16000 Hz
  withCArray fftRes $ \ptr -> do
    res <-
      (,,)
        <$> numLoopState 0 bassIndex 0 (\acc i -> (acc +) . abs . realToFrac <$> peekElemOff ptr i)
        <*> numLoopState bassIndex midIndex 0 (\acc i -> (acc +) . abs . realToFrac <$> peekElemOff ptr i)
        <*> numLoopState midIndex highIndex 0 (\acc i -> (acc +) . abs . realToFrac <$> peekElemOff ptr i)
    atomically $ writeTVar stm (Just res)
  isNothing <$> tryReadMVar shouldStop
