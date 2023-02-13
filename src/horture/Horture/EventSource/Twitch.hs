{-# LANGUAGE RankNTypes #-}

-- | Horture.EventSource.Twitch is an EventSource AND Controller, which allows
-- to dynamically adjust the priceranges for effects depending on user-defined
-- metrics.
module Horture.EventSource.Twitch where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Horture.Effect
import Horture.Event
import Horture.EventSource.Controller
import Horture.EventSource.EventSource

-- | CostAdjustFunction is a function taking in the deltatime since the effect
-- in question was triggered, the desired and current cost. It returns the
-- adjusted cost.
type CostAdjustFunction = (Double, Float, Float) -> Float

constCostFunction :: CostAdjustFunction
constCostFunction (_, bc, _) = bc

linearCostFunction :: CostAdjustFunction
linearCostFunction (dt, bc, cp)
  | dt > 5 =
    let np = cp * 0.8
     in if np < bc then bc else np
  | otherwise = cp + 25

linearIncreaseFunction :: CostAdjustFunction
linearIncreaseFunction (_, _, cp) = cp + 25

decayFunction :: CostAdjustFunction
decayFunction (dt, bc, cp)
  | dt > 5 =
    let np = cp * 0.8
     in if np < bc then bc else np
  | otherwise = cp

-- | TwitchEventSoucreState tracks the current costs for each effect identified
-- by a string id together with their deltatime since last usage.
data TwitchEventSourceState = TwitchEventSourceState
  { _twitchEventSourceStateEventCosts :: !(TVar (Map.Map Text (Double, Float, Float))),
    _twitchEventSourceStateStepIncrease :: !CostAdjustFunction,
    _twitchEventSourceStateStepDecrease :: !CostAdjustFunction
  }

makeFields ''TwitchEventSourceState

runTwitchEventSource ::
  forall effs x.
  (Members '[Logger, EventController] effs, LastMember IO (EventSink : effs)) =>
  TwitchEventSourceState ->
  Eff (EventSink : effs) x ->
  Eff (EventSink : effs) x
runTwitchEventSource s eff = evalState s runEff'
  where
    runEff' = do
      let interval = 1
      _tid <- startTicker' interval $ \s -> do
        let tvarCache = s ^. eventCosts
            stepDecreaseF = s ^. stepDecrease
            stepToBaseCost = applyCost (fromIntegral interval) stepDecreaseF
        liftIO . atomically . modifyTVar' tvarCache $ stepToBaseCost
        changeEventCost undefined undefined
      res <- reinterpret2 twitchEventSourceHandler eff
      -- liftIO $ killThread tid
      return res

twitchEventSourceHandler ::
  (Members '[State TwitchEventSourceState, EventSink, Logger, EventController] effs, LastMember IO effs) =>
  EventSink ~> Eff effs
twitchEventSourceHandler (SinkEvent ev) = do
  handleEventCost ev
  send $ SinkEvent ev

handleEventCost ::
  (Members '[State TwitchEventSourceState, Logger, EventController] effs, LastMember IO effs) =>
  Event ->
  Eff effs ()
handleEventCost (EventCommand _) = return ()
handleEventCost (EventEffect _ eff) = do
  let id = toTitle eff
  s <- get @TwitchEventSourceState
  let tvarCache = s ^. eventCosts
      stepIncreaseF = s ^. stepIncrease
      stepUpCurrentCost = applyAndResetTime stepIncreaseF
  void . liftIO . atomically . modifyTVar' tvarCache $ stepUpCurrentCost id
  Map.lookup id <$> liftIO (readTVarIO tvarCache) >>= \case
    Nothing -> return ()
    Just (_dt, _bp, cp) -> void $ changeEventCost id (round cp)

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
        threadDelay (interval * 1000 * 1000)
        action s
        loop
  liftIO . forkIO $ loop

startTicker' ::
  ( Members '[State TwitchEventSourceState] effs,
    LastMember IO effs
  ) =>
  Int ->
  Eff effs () ->
  Eff effs ()
startTicker' interval action = do
  let loop = do
        liftIO $ threadDelay (interval * 1000 * 1000)
        action
        loop
  loop

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
