module Horture.Error (HortureError (..)) where

data HortureError
  = HE !String
  | AudioSourceUnavailableErr
  | WindowEnvironmentInitializationErr !String
  | WindowEnvironmentQueryHortureErr
  deriving (Show)
