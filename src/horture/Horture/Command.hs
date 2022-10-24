module Horture.Command (Command (..)) where

data Command = Noop
             | Exit
  deriving (Show, Eq)
