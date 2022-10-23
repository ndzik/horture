{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Horture.EventSource.WebSocketClient
  ( hortureWSStaticClientApp,
  )
where

import Control.Concurrent.Chan
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Horture.Effect
import Horture.Event
import Horture.EventSource.EventSource
import Horture.EventSource.Random
import Horture.Server.Message
import Network.WebSockets
import qualified Twitch.EventSub.Event as TEvent

runWSEventSource ::
  (Members '[Reader StaticEffectRandomizerEnv, RandomizeEffect] effs, LastMember IO effs) =>
  Connection ->
  Chan Event ->
  Eff (EventSource : effs) x ->
  Eff effs x
runWSEventSource conn evChan = interpret $ do
  \case
    SourceEvent -> liftIO (receiveData @HortureServerMessage conn) >>= resolveServerMessageToEvent
    SinkEvent ev -> liftIO (writeChan evChan ev)

resolveServerMessageToEvent :: (Members '[Reader StaticEffectRandomizerEnv, RandomizeEffect] effs)
                            => HortureServerMessage -> Eff effs Event
resolveServerMessageToEvent HortureServerGarbage = return $ EventEffect Noop
resolveServerMessageToEvent (HortureEventSub ev) = resolveToEvent ev
  where
    resolveToEvent TEvent.ChannelPointsCustomRewardRedemptionAdd {..} = do
      let TEvent.Reward {..} = eventReward
      EventEffect <$> (effectFromTitle rrewardTitle >>= randomizeEffect)
    resolveToEvent _ = return $ EventEffect Noop

effectFromTitle ::
  (Members '[Reader StaticEffectRandomizerEnv] effs) =>
  Text ->
  Eff effs Effect
effectFromTitle title =
  asks @StaticEffectRandomizerEnv
    ( \m -> case Map.lookup title m of
        Nothing -> Noop
        Just (_, eff) -> eff
    )

hortureWSStaticClientApp :: Chan Event -> Map.Map Text (Text, Effect) -> ClientApp ()
hortureWSStaticClientApp evChan env conn =
  runM
    . runReader env
    . runStaticEffectRandomizer
    . runWSEventSource conn evChan
    $ eventSource
