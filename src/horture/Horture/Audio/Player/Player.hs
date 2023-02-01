module Horture.Audio.Player.Player where

import Data.ByteString
import Horture.Audio.Player.Effects

-- | The AudioPlayer describes the capability to output sound to the
-- environment.
class Monad m => AudioPlayer m where
  -- | init initializes the audioplayer sink.
  initAudio :: m Bool

  -- | play outputs the given Sound to the environment. It can either be
  -- statically known, generated or dynamically read sounddata.
  playAudio :: Sound StaticSoundEffect -> m ()

  -- | clear clears all playing audio.
  clearAudio :: m ()

  -- | deinit frees all required resources for the audioplayer.
  deinitAudio :: m ()

-- | Sound is an abstraction for sounddata used by the AudioPlayer.
data Sound k
  = StaticSound !Float !k
  | DynamicSound !FilePath
  | GeneratedSound !String !PCM

instance (Show k) => Show (Sound k) where
  show (StaticSound _ k) = show k
  show (DynamicSound fp) = unwords ["SoundFile:", fp]
  show (GeneratedSound name _) = name

-- | PCM describes a PCM stream which can be used to play a sound.
data PCM = PCM !ByteString !Int !Int !Int !Float

flashbangPeep :: PCM
flashbangPeep =
  let samples = pack [round $ (1 - i / sampleNum) * 124 * sin i | i <- [0 .. sampleNum]]
      sampleNum = 3 * fromIntegral @_ @Double sampleRate
      chans = 2
      sampleRate = 44100
      bitsPerSample = 8
      volume = 0.001
   in PCM samples chans sampleRate bitsPerSample volume
