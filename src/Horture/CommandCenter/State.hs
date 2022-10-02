module Horture.CommandCenter.State (CommandCenterState (..)) where

import Control.Concurrent.Chan.Synchronous
import Data.Default
import Graphics.X11
import Horture.Event

data CommandCenterState = CCState
  { _ccEventChan :: !(Maybe (Chan Event)),
    _ccCapturedWin :: !(Maybe (String, Window)),
    _ccGifs :: ![FilePath]
  }

instance Default CommandCenterState where
  def =
    CCState
      { _ccEventChan = Nothing,
        _ccCapturedWin = Nothing,
        _ccGifs = []
      }
