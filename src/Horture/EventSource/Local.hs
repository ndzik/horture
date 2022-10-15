{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Horture.EventSource.Local
  ( hortureLocalEventSource,
  )
where

import Control.Concurrent.Chan
import Control.Monad.Freer
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Horture.Effect
import Horture.Event
import Horture.EventSource.EventSource
import Horture.EventSource.Random

-- | runLocalEventSource creates events out of thin air using the effect
-- randomizer from the context.
runLocalEventSource :: (Members '[RandomizeEffect] effs, LastMember IO effs) => Chan Event -> Eff (EventSource : effs) x -> Eff effs x
runLocalEventSource evChan = interpret $ do
  \case
    SourceEvent -> EventEffect <$> randomizeEffect Noop
    SinkEvent ev -> liftIO $ writeChan evChan ev

hortureLocalEventSource :: Chan Event -> [FilePath] -> IO ()
hortureLocalEventSource evChan env =
  runM
    . runReader env
    . runAnyEffectRandomizer
    . runLocalEventSource evChan
    $ eventSource
