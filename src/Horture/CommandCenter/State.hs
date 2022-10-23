{-# LANGUAGE NumericUnderscores #-}

module Horture.CommandCenter.State (CommandCenterState (..)) where

import Brick.BChan
import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan.Synchronous
import Data.Default
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Graphics.X11
import Horture.CommandCenter.Event
import Horture.Effect
import Horture.Event
import Horture.EventSource.Controller
import Horture.Loader.Asset

data CommandCenterState = CCState
  { _ccEventChan :: !(Maybe (Chan Event)),
    _brickEventChan :: !(Maybe (BChan CommandCenterEvent)),
    _ccCapturedWin :: !(Maybe (String, Window)),
    _ccControllerChans :: !(Maybe (Chan EventControllerInput, Chan EventControllerResponse)),
    _ccLog :: ![Text],
    _ccGifs :: ![FilePath],
    _ccPreloadedGifs :: ![(FilePath, Asset)],
    _ccRegisteredEffects :: !(Map.Map Text (Text, Effect)),
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
        _ccControllerChans = Nothing,
        _ccRegisteredEffects = Map.empty,
        _ccLog = [],
        _ccGifs = [],
        _ccPreloadedGifs = [],
        _ccTIDsToClean = [],
        _ccTimeout = 1_000_000
      }
