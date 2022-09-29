module Horture.Event (Event (..)) where

import Horture.Effect

data Event
  = ChatEvent !Effect
  | CommandEvent
  deriving (Eq)
