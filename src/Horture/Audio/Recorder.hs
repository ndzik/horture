{-# LANGUAGE TemplateHaskell #-}

module Horture.Audio.Recorder
  ( AudioRecorder (..),
    FFTSnapshot,
    BandState (..),
    AllBands (..),
    abBass,
    abMid,
    abHigh,
    bsBaselineShort,
    bsBaselineLong,
    bsValue,
  )
where

import Control.Lens
import Data.Default

-- | (BassAmp, MidAmp, HighAmp)
type FFTSnapshot = (Float, Float, Float)

data AllBands = AllBands
  { _abBass :: !BandState,
    _abMid :: !BandState,
    _abHigh :: !BandState
  }

data BandState = BandState
  { _bsBaselineShort :: !Float,
    _bsBaselineLong :: !Float,
    _bsValue :: !Float
  }

instance Default BandState where
  def = BandState (-60) (-60) (-60)

instance Default AllBands where
  def = AllBands def def def

-- | An AudioRecorder records the audio from some source/sink and provides an
-- interface to query information about the live audio.
class (Monad m) => AudioRecorder m where
  -- | starts recording from some preconfigured source/sink.
  startRecording :: m ()

  -- | stops the recording.
  stopRecording :: m ()

  -- | currentPeak returns a triple of
  -- (SamplingRate, Dominating Frequency, Amplitude).
  currentFFTPeak :: m FFTSnapshot

  -- | withRecording allows to safely acquire the audio recorder resource and
  -- guarantee a release for blocked resources in async exception cases.
  withRecording :: m a -> m ()

makeLenses ''BandState
makeLenses ''AllBands
