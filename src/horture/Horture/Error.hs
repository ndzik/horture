module Horture.Error (HortureError (..)) where

import Horture.Audio.Player.Error

data HortureError
  = HE !String
  | AudioSinkUnavailableErr !String
  | AudioSinkInitializationErr
  | AudioSinkPlayErr !AudioPlayerError
  | WindowEnvironmentInitializationErr !String
  | WindowEnvironmentQueryHortureErr
  deriving (Show)
