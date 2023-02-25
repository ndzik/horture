-- | The EventSource.Controller allows to configure an EventSource, e.g. which
-- events are enabled and can be received from the EventSource itself.
module Horture.EventSource.Controller.Controller
  ( EventController (..),
    listAllEvents,
    enableEvents,
    purgeAllEvents,
    changeEventCost,
    controlEventSource,
    disableAllEvents,
    enableAllEvents,
    EventControllerResponse (..),
    EventControllerInput (..),
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Monad (void, when)
import Control.Monad.Extra (unlessM)
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
  -- The EventController shall synchronize its internal state when this
  -- function is called.
  ListAllEvents :: EventController [(Text, (Text, Effect))]
  -- | Enables the given [(Name, Effect, Int)] event pairs on the controlled
  -- source associated with the given cost, if applicable. Bool indicates
  -- success.
  EnableEvents :: [(Text, Effect, Int)] -> EventController Bool
  -- | Changes the cost for an event id using the given value.
  ChangeEventCost :: Text -> Int -> EventController Bool
  -- | Purge all enabled events. This disables all events on the controlled
  -- event source.
  PurgeAllEvents :: EventController Bool
  -- | Disables all events. Semantically this is not supposed to terminally
  -- remove all events but rather disallowing them to appear in subscriptions
  -- related to those events.
  DisableAllEvents :: EventController Bool
  -- | Enables all events. This is supposed to register AND enable events if
  -- they are not yet registered on the backend service, otherwise they are
  -- just getting enabled.
  EnableAllEvents :: EventController Bool

makeEffect ''EventController

data EventControllerInput
  = InputListEvents
  | InputEnable ![(Text, Effect, Int)]
  | InputPurgeAll
  | InputDisableAll
  | InputEnableAll
  | InputTerminate
  | InputChange !Text !Int
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
  synchronizeEventState
  -- Enter control loop.
  go
  where
    go ::
      (Members '[EventController, Logger] effs, LastMember IO effs) =>
      Eff effs ()
    go = do
      let cont = return True
          abort = return False
      res <-
        liftIO (readChan inputChan) >>= \case
          InputListEvents -> listAllEvents >>= liftIO . writeChan resChan . ListEvents >> cont
          InputEnable t -> enableEvents t >>= liftIO . writeChan resChan . Enable >> cont
          InputPurgeAll -> purgeAllEvents >>= liftIO . writeChan resChan . PurgeAll >> cont
          InputChange t c -> changeEventCost t c >>= liftIO . writeChan resChan . Enable >> cont
          InputEnableAll -> enableAllEvents >>= liftIO . writeChan resChan . Enable >> cont
          InputDisableAll -> disableAllEvents >>= liftIO . writeChan resChan . Enable >> cont
          InputTerminate -> disableAllEvents >>= liftIO . writeChan resChan . PurgeAll >> abort
      when res go

-- | Synchronizes the state of the EventController, with the backend service.
synchronizeEventState ::
  (Members '[EventController, Logger] effs, LastMember IO effs) =>
  Eff effs ()
synchronizeEventState = do
  void listAllEvents
  -- First purge all existing events on the eventsource.
  unlessM disableAllEvents $ logWarn "unable to clear EventSource initially"
