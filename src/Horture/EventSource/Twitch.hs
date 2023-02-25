{-# LANGUAGE RankNTypes #-}

-- | Horture.EventSource.Twitch is an EventSource AND Controller, which allows
-- to dynamically adjust the priceranges for effects depending on user-defined
-- metrics.
module Horture.EventSource.Twitch where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
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

-- TODO: Add a stepfunction for decaying.
decayFunction :: Double -> CostAdjustFunction
decayFunction timeTillDecay = decay
  where
    decay (dt, bc, cp)
      | dt > timeTillDecay =
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
  (Members '[Logger] effs, LastMember IO (EventSink : effs)) =>
  TwitchEventSourceState ->
  Chan EventControllerInput ->
  Chan EventControllerResponse ->
  Eff (EventSink : effs) x ->
  Eff (EventSink : effs) x
runTwitchEventSource s ic rc eff = runTwitchControllerWrapper ic rc . evalState s $ runEff'
  where
    runEff' = do
      let interval = 1
      tid <- startTicker interval (twitchControllerWrapper ic rc) $ do
        let tvarCache = s ^. eventCosts
            stepDecreaseF = s ^. stepDecrease
            stepToBaseCost = applyCost (fromIntegral interval) stepDecreaseF
        registeredEvents <- Map.fromList <$> listAllEvents
        costsToUpdate <- liftIO . atomically $ do
          oldCosts <- readTVar tvarCache
          let newCosts = stepToBaseCost oldCosts
              appendIfChanged acc k (_, _, v) =
                let newValue = round v
                    res =
                      Map.lookup k registeredEvents >>= \(backendId, _) -> do
                        case Map.lookup k oldCosts of
                          Just (_, _, v') | round v' /= newValue -> Just (backendId, newValue)
                          _otherwise -> Nothing
                 in case res of
                      Just v -> v : acc
                      Nothing -> acc
              toUpdate = Map.foldlWithKey' appendIfChanged [] newCosts
          writeTVar tvarCache newCosts
          return toUpdate
        mapM_ (uncurry changeEventCost) costsToUpdate
      res <- reinterpret3 twitchEventSourceHandler eff
      liftIO $ killThread tid
      return res

twitchControllerWrapper ::
  Chan EventControllerInput ->
  Chan EventControllerResponse ->
  Eff '[EventController, IO] a ->
  IO a
twitchControllerWrapper ic rc = runM . runTwitchControllerWrapper ic rc

runTwitchControllerWrapper ::
  LastMember IO effs =>
  Chan EventControllerInput ->
  Chan EventControllerResponse ->
  Eff (EventController : effs) ~> Eff effs
runTwitchControllerWrapper ic rc = interpret $ \case
  ChangeEventCost id c -> do
    liftIO . writeChan ic $ InputChange id c
    (liftIO . readChan $ rc) >>= \case
      Enable t -> return t
      _else -> error "unexpected event controller response"
  ListAllEvents -> do
    liftIO . writeChan ic $ InputListEvents
    -- TODO: This blocks for some reason?
    (liftIO . readChan $ rc) >>= \case
      ListEvents e -> return e
      _else -> error "unexpected event controller response"
  _otherwise -> error "unhandled effect in TwitchControllerWrapper"

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
  registeredEvents <- Map.fromList <$> listAllEvents
  backendId <- case Map.lookup id registeredEvents of
    Just (backendId, _) -> return backendId
    Nothing -> do
      logError $ "unregistered event encountered: " <> id
      -- TODO: Add Freer.Error implementation here.
      error "handling event cost: unregistered event encountered"
  liftIO (Map.lookup id <$> readTVarIO tvarCache) >>= \case
    Nothing -> return ()
    Just (_dt, _bp, cp) -> void $ changeEventCost backendId (round cp)

-- | startTicker starts a recurring handler invoked at each tick determined by
-- the interval given in seconds.
startTicker ::
  (LastMember IO effsA, LastMember IO effsB) =>
  -- Interval in seconds for ticker.
  Int ->
  -- Effects interpreter for the given action.
  (Eff effsB a -> IO ()) ->
  -- Action to run in the ticker.
  Eff effsB a ->
  Eff effsA ThreadId
startTicker interval runEff action = do
  let loop = do
        liftIO $ threadDelay (interval * 1000 * 1000)
        void action
        loop
  liftIO . forkIO . runEff $ loop

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
