module Horture.Audio.Player.Error where

data AudioPlayerError
  = StaticAudioSampleNotFoundErr
  | AudioPlayerSinkUnavailableErr
  deriving (Show)
