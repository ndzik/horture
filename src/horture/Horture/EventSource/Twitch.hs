-- | Horture.EventSource.Twitch is an EventSource AND Controller, which allows
-- to dynamically adjust the priceranges for effects depending on user-defined
-- metrics.
module Horture.EventSource.Twitch where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Horture.Effect
import Horture.Event
import Horture.EventSource.Controller
import Horture.EventSource.EventSource
import Horture.EventSource.Random

-- | CostAdjustFunction is a function taking in the deltatime since the effect
-- in question was triggered, the desired and current cost. It returns the
-- adjusted cost.
type CostAdjustFunction = (Double, Float, Float) -> Float

-- | TwitchEventSoucreState tracks the current costs for each effect identified
-- by a string id together with their deltatime since last usage.
data TwitchEventSourceState = TwitchEventSourceState
  { _twitchEventSourceStateEventCosts :: !(TVar (Map.Map Text (Double, Float, Float))),
    _twitchEventSourceStateStepIncrease :: !CostAdjustFunction,
    _twitchEventSourceStateStepDecrease :: !CostAdjustFunction
  }

makeFields ''TwitchEventSourceState

handleTwitchEventSource ::
  ( Members
      '[ Reader StaticEffectRandomizerEnv,
         RandomizeEffect,
         State TwitchControllerState,
         State TwitchEventSourceState,
         Logger
       ]
      effs,
    LastMember IO effs
  ) =>
  Eff (EventSource : effs) ~> Eff (EventSource : effs)
handleTwitchEventSource eff = do
  let interval = 1
  tid <- startTicker interval $ \s -> do
    let tvarCache = s ^. eventCosts
        stepDecreaseF = s ^. stepDecrease
        stepToBaseCost = applyCost (fromIntegral interval) stepDecreaseF
    atomically . modifyTVar' tvarCache $ stepToBaseCost
  res <-
    interpose
      ( \case
          SourceEvent -> do
            -- We are just interested in intercepting the events which were
            -- generated before, so we are fine with generating Noop events
            -- for this wrapper.
            return $ EventEffect "" Noop
          SinkEvent ev -> handleEventCost ev
      )
      eff
  let liftIO' :: (LastMember IO effs) => IO a -> Eff effs a
      liftIO' = liftIO
  liftIO' $ killThread tid
  return res

handleEventCost ::
  (Members '[State TwitchEventSourceState] effs, LastMember IO effs) =>
  Event ->
  Eff (EventSource : effs) ()
handleEventCost (EventCommand _) = return ()
handleEventCost (EventEffect id _) = do
  s <- get @TwitchEventSourceState
  let tvarCache = s ^. eventCosts
      stepIncreaseF = s ^. stepIncrease
      stepUpCurrentCost = applyAndResetTime stepIncreaseF
      liftIO' :: (LastMember IO effs) => IO a -> Eff effs a
      liftIO' = liftIO
  void . liftIO' . atomically . modifyTVar' tvarCache $ stepUpCurrentCost id

-- | startTicker starts a recurring handler invoked at each tick determined by
-- the interval given in seconds.
startTicker ::
  ( Members '[State TwitchEventSourceState] effs,
    LastMember IO effs
  ) =>
  Int ->
  (TwitchEventSourceState -> IO ()) ->
  Eff effs ThreadId
startTicker interval action = do
  s <- get @TwitchEventSourceState
  let loop = do
        threadDelay (interval * 1000)
        action s
        loop
  liftIO . forkIO $ loop

-- | applyAndResetTime applies the given cost function to the entry of the
-- given map and resets its timinginformation.
applyAndResetTime ::
  CostAdjustFunction ->
  Text ->
  Map.Map Text (Double, Float, Float) ->
  Map.Map Text (Double, Float, Float)
applyAndResetTime f = Map.update (\e@(_dt, bc, _cc) -> Just (0, bc, f e))

-- | applyCost applies the given costfunctions to all entries in the given map.
applyCost ::
  Double ->
  CostAdjustFunction ->
  Map.Map Text (Double, Float, Float) ->
  Map.Map Text (Double, Float, Float)
applyCost dt f = Map.map (\e@(ct, bc, _cc) -> (ct + dt, bc, f e))
