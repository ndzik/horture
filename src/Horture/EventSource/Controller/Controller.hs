{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The EventSource.Controller allows to configure an EventSource, e.g. which
-- events are enabled and can be received from the EventSource itself.
module Horture.EventSource.Controller.Controller
  ( EventController (..),
    listAllEvents,
    enableEvent,
    purgeAllEvents,
    controlEventSource,
    EventControllerResponse (..),
    EventControllerInput (..),
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Horture.Effect
import Horture.EventSource.Logger

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

data EventControllerInput
  = InputListEvents
  | InputEnable !(Text, Effect)
  | InputPurgeAll
  | InputTerminate
  deriving (Show)

data EventControllerResponse
  = ListEvents ![(Text, (Text, Effect))]
  | Enable !Bool
  | PurgeAll !Bool
  deriving (Show)

controlEventSource ::
  (Members '[EventController, Logger] effs, LastMember IO effs) =>
  Chan EventControllerInput ->
  Chan EventControllerResponse ->
  Eff effs ()
controlEventSource inputChan resChan = do
  -- First purge all existing events on the eventsource.
  purgeAllEvents >>= \case
    True -> return ()
    False -> logWarn "unable to clear EventSource initially"
  -- Enter control loop.
  go inputChan resChan
  where
    go ::
      (Members '[EventController, Logger] effs, LastMember IO effs) =>
      Chan EventControllerInput ->
      Chan EventControllerResponse ->
      Eff effs ()
    go ic rc = do
      res <-
        liftIO (readChan ic) >>= \case
          InputListEvents -> listAllEvents >>= liftIO . writeChan rc . ListEvents >> return True
          InputEnable t -> enableEvent t >>= liftIO . writeChan rc . Enable >> return True
          InputPurgeAll -> purgeAllEvents >>= liftIO . writeChan rc . PurgeAll >> return True
          InputTerminate -> purgeAllEvents >>= liftIO . writeChan rc . PurgeAll >> return False
      when res $ go ic rc
