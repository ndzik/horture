module Horture.CommandCenter.State (CommandCenterState (..)) where

import Data.Default

data CommandCenterState = CCState
  {
  }

instance Default CommandCenterState where
  def = CCState {}
