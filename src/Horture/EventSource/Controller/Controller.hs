{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | The EventSource.Controller allows to configure an EventSource, e.g. which
-- events are enabled and can be received from the EventSource itself.
module Horture.EventSource.Controller.Controller
  ( EventController (..),
    listAllEvents,
    enableEvent,
    purgeAllEvents,
  )
where

import Control.Monad.Freer.TH
import Data.Text (Text)
import Horture.Effect

data EventController a where
  -- | List all enabled events on the controlled source. This associates the
  -- title of the effect with the effect itself and a EventSource
  -- defined/controlled identifier.
  ListAllEvents :: EventController [(Text, (Text, Effect))]
  -- | Enables the given (Name, Effect) event pair on the controlled source.
  -- Bool indicates success.
  EnableEvent :: (Text, Effect) -> EventController Bool
  -- | Purge all enabled events. This disables all events on the controlled
  -- event source.
  PurgeAllEvents :: EventController Bool

makeEffect ''EventController
