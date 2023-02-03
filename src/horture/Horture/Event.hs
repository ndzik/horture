module Horture.Event (Event (..), PastEvent (..)) where

import Data.Text (Text, unpack)
import Horture.Command
import Horture.Effect

data Event
  = EventEffect !Text !Effect
  | EventCommand !Command
  deriving (Show)

data PastEvent
  = PastEvent !Text !Effect

instance Show PastEvent where
  show (PastEvent name eff) = unwords [unpack $ name <> ":", unpack . toTitle $ eff]
