module Horture.EventSource.EventSource
  ( EventSource (..),
    EventSink (..),
    RandomizeEffect (..),
    randomizeEffect,
    sourceEvent,
    sinkEvent,
    eventSource,
  )
where

import Control.Concurrent.STM
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.TH
import Control.Monad.IO.Class
import Horture.Effect
import Horture.Event

data EventSource a where
  SourceEvent :: EventSource Event

data EventSink a where
  SinkEvent :: Event -> EventSink ()

makeEffect ''EventSource
makeEffect ''EventSink

data RandomizeEffect a where
  RandomizeEffect :: Effect -> RandomizeEffect Effect

makeEffect ''RandomizeEffect

-- | eventSource sources events from some source and passes them to a sink.
eventSource :: (Members '[Reader (TVar Bool), EventSource, EventSink] effs, LastMember IO effs) => Eff effs ()
eventSource =
  sourceEvent >>= \ev ->
    -- Only if we are enabled do we push the newly aquired event to the sink.
    -- Otherwise we drop it.
    ask @(TVar Bool) >>= liftIO . readTVarIO >>= \case
      True -> sinkEvent ev >> eventSource
      False -> eventSource
