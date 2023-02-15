{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Horture.EventSource.WebSocketClient
  ( hortureWSStaticClientApp,
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Lens
import Control.Monad.Freer as F
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Horture.CommandCenter.Event
import Horture.Effect
import Horture.Event
import Horture.EventSource.EventSource
import Horture.EventSource.Logger
import Horture.EventSource.Random
import Horture.EventSource.Twitch
import Horture.Server.Message
import Network.WebSockets
import qualified Twitch.EventSub.Event as TEvent
import qualified Twitch.EventSub.Notification as TEvent
import Horture.EventSource.Controller.Controller (EventControllerInput)

runWSEventSource ::
  forall effs x.
  (Members '[Reader StaticEffectRandomizerEnv, RandomizeEffect] effs, LastMember IO effs) =>
  Connection ->
  Eff (EventSource : effs) x ->
  Eff effs x
runWSEventSource conn = interpret (wsEventSourceHandler conn)

wsEventSourceHandler ::
  (Members '[Reader StaticEffectRandomizerEnv, RandomizeEffect] effs, LastMember IO effs) =>
  Connection ->
  (EventSource ~> Eff effs)
wsEventSourceHandler conn SourceEvent = liftIO (receiveData @HortureServerMessage conn) >>= resolveServerMessageToEvent

runWSEventSink ::
  forall effs x.
  (Members '[Reader StaticEffectRandomizerEnv, RandomizeEffect, Logger] effs, LastMember IO effs) =>
  Chan Event ->
  Eff (EventSink : effs) x ->
  Eff effs x
runWSEventSink evChan = interpret (wsEventSinkHandler evChan)

wsEventSinkHandler ::
  (Members '[Reader StaticEffectRandomizerEnv, RandomizeEffect, Logger] effs, LastMember IO effs) =>
  Chan Event ->
  (EventSink ~> Eff effs)
wsEventSinkHandler evChan (SinkEvent ev) = do
  logInfo . pack . unwords $ ["EventSinkHandler:", "SinkEvent", unpack . toTitle $ ev]
  liftIO (writeChan evChan ev)

resolveServerMessageToEvent ::
  (Members '[Reader StaticEffectRandomizerEnv, RandomizeEffect] effs) =>
  HortureServerMessage ->
  Eff effs Event
resolveServerMessageToEvent HortureServerGarbage = return $ EventEffect "" Noop
resolveServerMessageToEvent (HortureEventSub ev) = resolveToEvent ev
  where
    resolveToEvent (TEvent.EventNotification _ TEvent.ChannelPointsCustomRewardRedemptionAdd {..}) = do
      let TEvent.Reward {..} = eventReward
      EventEffect eventUserName <$> (effectFromTitle rrewardTitle >>= randomizeEffect)
    resolveToEvent _ = return $ EventEffect "" Noop

effectFromTitle ::
  (Members '[Reader StaticEffectRandomizerEnv] effs) =>
  Text ->
  Eff effs Effect
effectFromTitle title =
  asks @StaticEffectRandomizerEnv (^. registeredEffects) >>= \m -> case Map.lookup title m of
    Nothing -> return Noop
    Just (_, eff) -> return eff

hortureWSStaticClientApp ::
  [(Text, Effect, Int)] ->
  Text ->
  Chan Event ->
  Chan CommandCenterEvent ->
  Chan EventControllerInput ->
  StaticEffectRandomizerEnv ->
  TVar Bool ->
  ClientApp ()
hortureWSStaticClientApp events bid evChan ccChan ecInput env enabled conn = do
  liftIO $ sendTextData conn (HortureAuthorization bid)
  esTvar <- liftIO . newTVarIO . buildFromEvents $ events
  runM
    . runHortureChannelLogger ccChan
    . runReader env
    . runReader enabled
    . runStaticEffectRandomizer
    . runWSEventSink evChan
    . runTwitchEventSource (TwitchEventSourceState esTvar linearIncreaseFunction decayFunction) ecInput
    . runWSEventSource conn
    $ eventSource

buildFromEvents :: [(Text, b, Int)] -> Map.Map Text (Double, Float, Float)
buildFromEvents =
  Map.fromList
    . map
      ( \(title, _, baseCost) ->
          (title, (0, fromIntegral baseCost, fromIntegral baseCost))
      )
