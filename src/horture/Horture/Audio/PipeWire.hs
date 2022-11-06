{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
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
import Control.Monad.State
import Data.Array.CArray.Base
import Data.Maybe
import Data.Text (pack)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Horture.Audio.Recorder
import Horture.Error
import Horture.Horture
import Horture.Logging
import Horture.State
import Math.FFT
import Math.FFT.Base

deriving instance FFTWReal CFloat

type AudioCallback = CUInt -> CUInt -> CUInt -> Ptr CFloat -> IO Bool

instance (HortureLogger (Horture l hdl)) => AudioRecorder (Horture l hdl) where
  startRecording =
    gets (^. audioRecording) >>= \case
      Nothing -> do
        stopIt <- liftIO newEmptyMVar
        storage <- liftIO $ newTVarIO Nothing
        void . liftIO $ mkAudioCallback (onProcessAudio stopIt storage) >>= forkIO . runPipeWire
        audioRecording .= Just stopIt
        audioStorage .= storage
      Just _ -> stopRecording >> startRecording
  stopRecording =
    gets (^. audioRecording) >>= \case
      Nothing -> return ()
      Just stopIt -> liftIO (putMVar stopIt ()) >> audioRecording .= Nothing
  currentFFTPeak =
    gets (^. audioStorage) >>= liftIO . readTVarIO >>= \case
      Nothing -> (logWarn . pack . show $ AudioSourceUnavailableErr) >> return (0, 0, 0)
      Just res -> (logInfo . pack . show $ res) >> return res

foreign import ccall "wrapper"
  mkAudioCallback :: AudioCallback -> IO (FunPtr AudioCallback)

foreign import ccall "record.c run"
  runPipeWire :: FunPtr AudioCallback -> IO ()

-- | onProcessAudio is the audio callback passed to pipewire when a new audio
-- sample is ready to be processed. It is parameterized on an MVar signal
-- indicating whether the processing is supposed to stop and a TVar as storage
-- for the audioprocessing result.
onProcessAudio :: MVar () -> TVar (Maybe FFTSnapshot) -> AudioCallback
onProcessAudio shouldStop stm sampleRate _nChannels nSamples samples = do
  cSamples <- CArray (0 :: Int) (fromIntegral $ nSamples - 1) (fromIntegral nSamples) <$> newForeignPtr_ samples
  let fftRes = dst1 cSamples
  (i, amp) <- withCArray fftRes $ \ptr -> do
    numLoopState 0 (fromIntegral $ nSamples - 1) (0, 0) $ \acc i -> max acc . (i,) <$> peekElemOff ptr i
  atomically $ writeTVar stm (Just (fromIntegral sampleRate, toFrequency i, realToFrac amp))
  isNothing <$> tryReadMVar shouldStop
  where
    max :: (Int, CFloat) -> (Int, CFloat) -> (Int, CFloat)
    max (i, v) (j, w)
      | abs v >= abs w = (i, abs v)
      | otherwise = (j, abs w)
    toFrequency :: Int -> Double
    toFrequency i = (fromIntegral i / n) * nyq
      where
        nyq = fromIntegral sampleRate / 2
        n = fromIntegral nSamples / 2
