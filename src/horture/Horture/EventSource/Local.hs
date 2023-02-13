module Horture.EventSource.Local
  ( hortureLocalEventSource,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM (TVar)
import Control.Monad.Freer
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Horture.Effect
import Horture.Event
import Horture.EventSource.EventSource
import Horture.EventSource.Random

-- | runLocalEventSource creates events out of thin air using the effect
-- randomizer from the context.
runLocalEventSource ::
  (Members '[RandomizeEffect] effs, LastMember IO effs) =>
  Int ->
  Eff (EventSource : effs) x ->
  Eff effs x
runLocalEventSource timeout = interpret $ do
  \case
    SourceEvent -> EventEffect "DebugUser" <$> randomizeEffect Noop <* liftIO (threadDelay timeout)

-- | runLocalEventSink passes given events to some sink.
runLocalEventSink ::
  (Members '[RandomizeEffect] effs, LastMember IO effs) =>
  Chan Event ->
  Eff (EventSink : effs) x ->
  Eff effs x
runLocalEventSink evChan = interpret $ do
  \case
    SinkEvent ev -> do
      liftIO $ writeChan evChan ev

hortureLocalEventSource :: Int -> Chan Event -> [FilePath] -> TVar Bool -> IO ()
hortureLocalEventSource timeout evChan env enabled =
  runM
    . runReader env
    . runReader enabled
    . runAnyEffectRandomizer
    . runLocalEventSource timeout
    . runLocalEventSink evChan
    $ eventSource
