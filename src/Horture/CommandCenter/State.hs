module Horture.CommandCenter.State (CommandCenterState (..)) where

import Control.Concurrent.Chan.Synchronous
import Data.Default
import Horture.Event

newtype CommandCenterState = CCState
  { _ccEventChan :: Maybe (Chan Event)
  }

instance Default CommandCenterState where
  def = CCState { _ccEventChan = Nothing }
