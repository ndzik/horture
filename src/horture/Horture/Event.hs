module Horture.Event (Event (..), PastEvent (..)) where

import Horture.Command
import Horture.Effect
import Data.Text (Text)

data Event
  = EventEffect !Text !Effect
  | EventCommand !Command
  deriving (Show)

data PastEvent
  = PastEvent !Text !Effect
