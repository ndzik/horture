{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Horture.EventSource.EventSource
  ( EventSource (..),
    RandomizeEffect (..),
    randomizeEffect,
    sourceEvent,
    sinkEvent,
    eventSource,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Horture.Effect
import Horture.Event

data EventSource a where
  SourceEvent :: EventSource Event
  SinkEvent :: Event -> EventSource ()

makeEffect ''EventSource

data RandomizeEffect a where
  RandomizeEffect :: Effect -> RandomizeEffect Effect

makeEffect ''RandomizeEffect

-- | eventSource sources events from some source and passes them to a sink.
eventSource :: (Members '[EventSource] effs) => Eff effs ()
eventSource = sourceEvent >>= sinkEvent >> eventSource
