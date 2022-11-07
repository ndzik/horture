{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Horture.EventSource.WebSocketClient
  ( hortureWSStaticClientApp,
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Lens
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
import qualified Twitch.EventSub.Notification as TEvent

runWSEventSource ::
  (Members '[Reader StaticEffectRandomizerEnv, RandomizeEffect] effs, LastMember IO effs) =>
  Connection ->
  Chan Event ->
  Eff (EventSource : effs) x ->
  Eff effs x
runWSEventSource conn evChan = do
  interpret $ do
    \case
      SourceEvent -> liftIO (receiveData @HortureServerMessage conn) >>= resolveServerMessageToEvent
      SinkEvent ev -> liftIO (writeChan evChan ev)

resolveServerMessageToEvent ::
  (Members '[Reader StaticEffectRandomizerEnv, RandomizeEffect] effs) =>
  HortureServerMessage ->
  Eff effs Event
resolveServerMessageToEvent HortureServerGarbage = return $ EventEffect Noop
resolveServerMessageToEvent (HortureEventSub ev) = resolveToEvent ev
  where
    resolveToEvent (TEvent.EventNotification _ TEvent.ChannelPointsCustomRewardRedemptionAdd {..}) = do
      let TEvent.Reward {..} = eventReward
      EventEffect <$> (effectFromTitle rrewardTitle >>= randomizeEffect)
    resolveToEvent _ = return $ EventEffect Noop

effectFromTitle ::
  (Members '[Reader StaticEffectRandomizerEnv] effs) =>
  Text ->
  Eff effs Effect
effectFromTitle title =
  asks @StaticEffectRandomizerEnv (^. registeredEffects) >>= \m -> case Map.lookup title m of
    Nothing -> return Noop
    Just (_, eff) -> return eff

hortureWSStaticClientApp :: Text -> Chan Event -> StaticEffectRandomizerEnv -> ClientApp ()
hortureWSStaticClientApp bid evChan env conn = do
  liftIO $ sendTextData conn (HortureAuthorization bid)
  runM
    . runReader env
    . runStaticEffectRandomizer
    . runWSEventSource conn evChan
    $ eventSource
