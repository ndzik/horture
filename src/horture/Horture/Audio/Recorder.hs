module Horture.Audio.Recorder (AudioRecorder (..), FFTSnapshot) where

-- | (BassAmp, MidAmp, HighAmp)
type FFTSnapshot = (Double, Double, Double)

-- | An AudioRecorder records the audio from some source/sink and provides an
-- interface to query information about the live audio.
class Monad m => AudioRecorder m where
  -- | starts recording from some preconfigured source/sink.
  startRecording :: m ()

  -- | stops the recording.
  stopRecording :: m ()

  -- | currentPeak returns a triple of
  -- (SamplingRate, Dominating Frequency, Amplitude).
  currentFFTPeak :: m FFTSnapshot
