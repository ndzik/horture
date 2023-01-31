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
  = StaticSound !k
  | DynamicSound !FilePath
  | GeneratedSound !String !PCM

-- | PCM describes a PCM stream which can be used to play a sound.
data PCM = PCM !ByteString !Int !Int !Int
