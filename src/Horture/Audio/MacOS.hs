{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Horture.Audio.MacOS
  ( AudioRecorderEnv (..),
    AudioPlayerEnv (..),
    AudioPlayerState (..),
    nativeInitPlayer,
    nativeDeinitPlayer,
    nativePlayAudio,
    nativeClearAudio,
    nativeStartRecording,
    nativeStopRecording,
    nativeCurrentFFT,
    nativeInitRecorder,
    nativeDeinitRecorder,
  )
where

import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Foreign hiding (void)
import Foreign.C
import Horture.Audio.Player (StaticSoundEffect)
import Horture.Audio.Player.Player

data APHandle

data ARHandle

foreign import ccall unsafe "audio_bridge.h ap_create"
  c_ap_create :: IO (Ptr APHandle)

foreign import ccall unsafe "audio_bridge.h ap_destroy"
  c_ap_destroy :: Ptr APHandle -> IO ()

foreign import ccall unsafe "audio_bridge.h ap_play_pcm"
  c_ap_play_pcm :: Ptr APHandle -> Ptr () -> CInt -> CInt -> CInt -> CInt -> CFloat -> IO CInt

foreign import ccall unsafe "audio_bridge.h ap_stop_all"
  c_ap_stop_all :: Ptr APHandle -> IO ()

foreign import ccall unsafe "audio_bridge.h ar_create"
  c_ar_create :: IO (Ptr ARHandle)

foreign import ccall unsafe "audio_bridge.h ar_destroy"
  c_ar_destroy :: Ptr ARHandle -> IO ()

foreign import ccall unsafe "audio_bridge.h ar_start"
  c_ar_start :: Ptr ARHandle -> CULong -> IO CInt

foreign import ccall unsafe "audio_bridge.h ar_stop"
  c_ar_stop :: Ptr ARHandle -> IO ()

foreign import ccall unsafe "audio_bridge.h ar_current_fft"
  c_ar_fft :: Ptr ARHandle -> Ptr CDouble -> IO CInt

data AudioPlayerEnv = AudioPlayerEnv
  {
  }

data AudioPlayerState = AudioPlayerState
  { apHandle :: Ptr APHandle,
    apSounds :: Map.Map String (Sound StaticSoundEffect)
  }

nativeInitPlayer :: IO AudioPlayerState
nativeInitPlayer = do
  h <- c_ap_create
  pure $ AudioPlayerState h Map.empty

nativeDeinitPlayer :: AudioPlayerState -> IO ()
nativeDeinitPlayer (AudioPlayerState h _) = c_ap_destroy h

nativePlayAudio :: AudioPlayerState -> Sound StaticSoundEffect -> IO ()
nativePlayAudio (AudioPlayerState _ _) (StaticSound _ _) = pure ()
nativePlayAudio (AudioPlayerState h _) (GeneratedSound _ (PCM bs ch sr bps vol)) =
  BS.useAsCStringLen bs $ \(p, len) ->
    void $ c_ap_play_pcm h (castPtr p) (fromIntegral len) (fromIntegral ch) (fromIntegral sr) (fromIntegral bps) (realToFrac vol)
nativePlayAudio (AudioPlayerState _ _) (DynamicSound _) = pure () -- load & decode first (out of scope)

nativeClearAudio :: AudioPlayerState -> IO ()
nativeClearAudio (AudioPlayerState h _) = c_ap_stop_all h

data AudioRecorderEnv = AudioRecorderEnv
  { arHandle :: Ptr ARHandle,
    windowID :: Word64
  }

nativeInitRecorder :: Word64 -> IO AudioRecorderEnv
nativeInitRecorder wid = do
  h <- c_ar_create
  pure $ AudioRecorderEnv h wid

nativeDeinitRecorder :: AudioRecorderEnv -> IO ()
nativeDeinitRecorder (AudioRecorderEnv h _) = c_ar_destroy h

nativeStartRecording :: AudioRecorderEnv -> IO ()
nativeStartRecording (AudioRecorderEnv h wid) = void $ c_ar_start h (fromIntegral wid)

nativeStopRecording :: AudioRecorderEnv -> IO ()
nativeStopRecording (AudioRecorderEnv h _) = c_ar_stop h

nativeCurrentFFT :: AudioRecorderEnv -> IO (Float, Float, Float)
nativeCurrentFFT (AudioRecorderEnv h _) = do
  allocaArray 3 $ \ptr -> do
    ok <- c_ar_fft h ptr
    if ok == 1
      then do
        [b, m, hg] <- map realToFrac <$> peekArray 3 ptr
        pure (b, m, hg)
      else pure (0, 0, 0)
