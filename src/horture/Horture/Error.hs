module Horture.Error (HortureError (..)) where

data HortureError
  = HE !String
  | AudioSourceUnavailableErr
  | AudioSinkUnavailableErr
  | WindowEnvironmentInitializationErr !String
  | WindowEnvironmentQueryHortureErr
  deriving (Show)
