{-# LANGUAGE NamedFieldPuns #-}

module Horture.EventSource.Controller.TwitchController
  ( handleTwitchEventController,
    runHortureTwitchEventController,
    TwitchControllerState (..),
    client,
    broadcasterId,
    clientEnv,
    events,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.Chan.Synchronous
import Control.Lens
import Control.Monad (join, void)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Horture.CommandCenter.Event
import Horture.Effect
import Horture.EventSource.Controller.Controller
import Horture.EventSource.Logger
import Servant.Client (ClientEnv, runClientM)
import Twitch.Rest
import Twitch.Rest.DataResponse
import Twitch.Rest.Types

data TwitchControllerState = TCS
  { _twitchControllerStateClient :: !TwitchChannelPointsClient,
    _twitchControllerStateBroadcasterId :: !Text,
    _twitchControllerStateClientEnv :: !ClientEnv
  }

makeFields ''TwitchControllerState

newtype TwitchControllerEnv = TCE
  { _twitchControllerEnvEvents :: Map.Map Text (Effect, Int)
  }

makeFields ''TwitchControllerEnv

type TwitchEventControllerEffects = '[Reader TwitchControllerEnv, State TwitchControllerState, Logger]

runHortureTwitchEventController ::
  Map.Map Text (Effect, Int) ->
  TwitchControllerState ->
  Chan CommandCenterEvent ->
  Chan EventControllerInput ->
  Chan EventControllerResponse ->
  IO ()
runHortureTwitchEventController effs s logChan inputChan responseChan =
  void $
    controlEventSource inputChan responseChan
      & handleTwitchEventController
      & runHortureChannelLogger logChan
      & runState s
      & runReader (TCE effs)
      & runM

handleTwitchEventController ::
  (Members TwitchEventControllerEffects effs, LastMember IO effs) =>
  Eff (EventController : effs) ~> Eff effs
handleTwitchEventController = interpret $ \case
  ListAllEvents -> getAllRewards'
  EnableEvents c -> putAndEnableRewards c
  ChangeEventCost id c -> changeCustomReward id c
  PurgeAllEvents -> deleteAllRewards
  EnableAllEvents -> enableAllRewards
  DisableAllEvents -> disableAllRewards

-- | Gets all rewards and synchronizes the local state cache with the backend.
getAllRewards' ::
  (Members TwitchEventControllerEffects effs, LastMember IO effs) =>
  Eff effs [(Text, (Text, Effect))]
getAllRewards' = do
  let mkEvent (Just eff) = eff
      mkEvent Nothing = Noop
  rewards <- Map.fromList . map (\(title, tid, meff) -> (title, (tid, mkEvent meff))) <$> getAllRewards
  return $ Map.toList rewards

-- | Enables all rewards which are available on the backend.
enableAllRewards ::
  (Members TwitchEventControllerEffects effs, LastMember IO effs) =>
  Eff effs Bool
enableAllRewards = do
  TwitchChannelPointsClient {updateCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  rewards <- getAllRewards
  responses <- liftIO $ mapConcurrently (go updateCustomReward env id) rewards
  all (== True) <$> mapM handleResponse responses
  where
    go call env id (rewardTitle, rewardId, _) = do
      let body = def {updatecustomrewardbodyIsEnabled = Just True}
      runClientM (call id rewardId body) env >>= \case
        Right (DataResponse _crs) -> return $ Right rewardTitle
        Left err -> return . Left . pack . show $ err
    handleResponse (Right title) = logInfo ("successfully enabled: " <> title) >> return True
    handleResponse (Left err) = logWarn ("error when enabling: " <> err) >> return False

-- | Disables all rewards which are available on the backend.
disableAllRewards ::
  (Members TwitchEventControllerEffects effs, LastMember IO effs) =>
  Eff effs Bool
disableAllRewards = do
  TwitchChannelPointsClient {updateCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  rewards <- getAllRewards
  responses <- liftIO $ mapConcurrently (go updateCustomReward env id) rewards
  all (== True) <$> mapM handleResponse responses
  where
    go call env id (rewardTitle, rewardId, _) = do
      let body = def {updatecustomrewardbodyIsEnabled = Just False}
      runClientM (call id rewardId body) env >>= \case
        Right (DataResponse _crs) -> return $ Right rewardTitle
        Left err -> return . Left . pack . show $ err
    handleResponse (Right title) = logInfo ("successfully disabled: " <> title) >> return True
    handleResponse (Left err) = logWarn ("error when disabling: " <> err) >> return False

-- | putAndEnableRewards registers the given [(Title, Effect)] pairs as custom
-- channel points rewards on the associated channel or enables them if they are
-- already registered but disabled.
putAndEnableRewards ::
  (Members TwitchEventControllerEffects effs, LastMember IO effs) =>
  [(Text, Effect, Int)] ->
  Eff effs Bool
putAndEnableRewards rewards = do
  rewards' <- identifyNewRewards rewards
  TwitchChannelPointsClient {createCustomReward, updateCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  res <- liftIO (mapConcurrently (createOrEnable createCustomReward updateCustomReward env id) rewards')
  let wasSuccessful (Left err) = logError err >> return False
      wasSuccessful (Right r) = (logInfo . pack . show $ r) >> return True
  all (== True) <$> (mapM wasSuccessful . join $ res)
  where
    identifyNewRewards effects = do
      let rewards' = Map.fromList . map (\(title, _, _) -> (title, "")) $ effects
      backendRewards <- Map.fromList . map (\(a, b, _) -> (a, b)) <$> getAllRewards
      let allRewards = Map.union backendRewards rewards'
          identifyNewReward (title, _eff, cost) =
            case Map.lookup title allRewards of
              Just "" -> (title, _eff, cost, Nothing)
              a -> (title, _eff, cost, a)
      return $ map identifyNewReward effects
    createOrEnable _create enable env id (name, _eff, _cost, Just effId) = do
      let body = def {updatecustomrewardbodyIsEnabled = Just True}
      runClientM (enable id effId body) env >>= \case
        Right (DataResponse _crs) -> return [Right (name, (effId, _eff))]
        Left err -> return [Left . pack . show $ err]
    createOrEnable create _enable env id (name, _eff, cost, Nothing) = do
      let body =
            def
              { createcustomrewardbodyTitle = name,
                createcustomrewardbodyCost = cost,
                createcustomrewardbodyBackgroundColor = Just $ effectToColor _eff
              }
      runClientM (create id body) env >>= \case
        Right (DataResponse crs) -> return $ map (\d -> Right (name, (getcustomrewardsdataId d, _eff))) crs
        Left err -> return [Left . pack . show $ err]

-- | Changes the custom reward by adjusting the price for the given reward id.
changeCustomReward ::
  (Members TwitchEventControllerEffects effs, LastMember IO effs) =>
  Text ->
  Int ->
  Eff effs Bool
changeCustomReward rewardId cost =
  changeCustomReward' rewardId def {updatecustomrewardbodyCost = Just cost}

-- | changeCustomReward' changes the reward identified by the rewardId using
-- the given UpdateCustomRewardBody.
changeCustomReward' ::
  (Members TwitchEventControllerEffects effs, LastMember IO effs) =>
  Text ->
  UpdateCustomRewardBody ->
  Eff effs Bool
changeCustomReward' rewardId body = do
  TwitchChannelPointsClient {updateCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  go (updateCustomReward id rewardId) env
  where
    go call env = do
      liftIO (runClientM (call body) env) >>= \case
        Right _ -> return True
        Left err -> logError (pack . show $ err) >> return False

-- | deleteAllRewards deletes all custom channel points rewards managed by this
-- client-id (application).
deleteAllRewards ::
  (Members '[Reader TwitchControllerEnv, State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  Eff effs Bool
deleteAllRewards = do
  TwitchChannelPointsClient {deleteCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  effs <- getAllRewards
  res <- liftIO $ mapConcurrently (go deleteCustomReward env id) effs
  foldrM resolveResult True res
  where
    go call env id (_, effId, _) = runClientM (call id effId) env
    resolveResult (Left err) _ = (logError . pack . show $ err) >> return False
    resolveResult _ False = return False
    resolveResult _ _ = return True

-- | getAllRewards returns all rewardids registered for this channel managed by
-- this client-id (application).
getAllRewards ::
  (Members TwitchEventControllerEffects effs, LastMember IO effs) =>
  Eff effs [(Text, Text, Maybe Effect)]
getAllRewards = do
  TwitchChannelPointsClient {getCustomRewards} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  effects <- asks @TwitchControllerEnv (^. events)
  let resolveEvent r =
        let title = getcustomrewardsdataTitle r
            id = getcustomrewardsdataId r
         in (title, id, fst <$> Map.lookup title effects)
  liftIO (runClientM (getCustomRewards id Nothing (Just True)) env) >>= \case
    Left err -> (logError . pack . show $ err) >> return []
    Right (DataResponse res) -> return $ map resolveEvent res
