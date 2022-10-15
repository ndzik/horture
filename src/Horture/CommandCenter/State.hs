{-# LANGUAGE NumericUnderscores #-}

module Horture.CommandCenter.State (CommandCenterState (..)) where

import Brick.BChan
import Control.Concurrent.Chan.Synchronous
import Data.Default
import Data.Text (Text)
import Graphics.X11
import Horture.CommandCenter.Event
import Horture.Event
import Control.Concurrent (ThreadId)

data CommandCenterState = CCState
  { _ccEventChan :: !(Maybe (Chan Event)),
    _brickEventChan :: !(Maybe (BChan CommandCenterEvent)),
    _ccCapturedWin :: !(Maybe (String, Window)),
    _ccLog :: ![Text],
    _ccGifs :: ![FilePath],
    _ccTIDsToClean :: ![ThreadId],
    -- | Timeout in microseconds for events to be generated. Only works in
    -- DEBUG mode.
    _ccTimeout :: !Int
  }

instance Default CommandCenterState where
  def =
    CCState
      { _ccEventChan = Nothing,
        _brickEventChan = Nothing,
        _ccCapturedWin = Nothing,
        _ccLog = [],
        _ccGifs = [],
        _ccTIDsToClean = [],
        _ccTimeout = 100_000
      }
