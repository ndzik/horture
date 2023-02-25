module Horture.Event (Event (..), PastEvent (..)) where

import Data.Text (Text, pack, unpack)
import Horture.Command
import Horture.Effect

data Event
  = EventEffect !Text !Effect
  | EventCommand !Command
  deriving (Show)

data PastEvent
  = -- | PastEvent describes a past event together with its birthtime in seconds
    PastEvent !Double !Text !Effect

instance Show PastEvent where
  show (PastEvent _ name eff) = unwords [unpack $ name <> ":", unpack . toTitle $ eff]

instance Entitled Event where
  toTitle (EventEffect _ eff) = toTitle eff
  toTitle (EventCommand cmd) = pack . show $ cmd
