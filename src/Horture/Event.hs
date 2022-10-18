module Horture.Event (Event (..)) where

import Horture.Command
import Horture.Effect

data Event
  = EventEffect !Effect
  | EventCommand !Command
  deriving (Show)
