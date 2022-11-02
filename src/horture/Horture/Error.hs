module Horture.Error (HortureError (..)) where

data HortureError
  = HE !String
  | WindowEnvironmentInitializationErr !String
  | WindowEnvironmentQueryHortureErr
  deriving (Show)
