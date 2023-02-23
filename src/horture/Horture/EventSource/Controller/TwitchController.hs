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
    _twitchControllerStateClientEnv :: !ClientEnv,
    -- | Events associates an effects title with its effect and twitch-id.
    _twitchControllerStateEvents :: !(Map.Map Text (Text, Effect))
  }

makeFields ''TwitchControllerState

runHortureTwitchEventController ::
  TwitchControllerState ->
  Chan CommandCenterEvent ->
  Chan EventControllerInput ->
  Chan EventControllerResponse ->
  IO ()
runHortureTwitchEventController s logChan inputChan responseChan =
  void $
    controlEventSource inputChan responseChan
      & handleTwitchEventController
      & runHortureChannelLogger logChan
      & runState s
      & runM

handleTwitchEventController ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
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
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  Eff effs [(Text, (Text, Effect))]
getAllRewards' = do
  rewards <- Map.fromList . map (\(title, tid) -> (title, (tid, AddRapidFire []))) <$> getAllRewards
  localRewards <- gets @TwitchControllerState (^. events)
  let newRewards = Map.union localRewards rewards
  modify @TwitchControllerState $ \s -> s & events .~ newRewards
  return $ Map.toList newRewards

-- | Enables all rewards which are available on the backend.
enableAllRewards ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  Eff effs Bool
enableAllRewards = do
  TwitchChannelPointsClient {updateCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  rewards <- getAllRewards
  responses <- liftIO $ mapConcurrently (go updateCustomReward env id) rewards
  all (== True) <$> mapM handleResponse responses
  where
    go call env id (rewardTitle, rewardId) = do
      let body = def {updatecustomrewardbodyIsEnabled = Just True}
      runClientM (call id rewardId body) env >>= \case
        Right (DataResponse _crs) -> return $ Right rewardTitle
        Left err -> return . Left . pack . show $ err
    handleResponse (Right title) = logInfo ("successfully enabled: " <> title) >> return True
    handleResponse (Left err) = logWarn ("error when enabling: " <> err) >> return False

-- | Disables all rewards which are available on the backend.
disableAllRewards ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  Eff effs Bool
disableAllRewards = do
  TwitchChannelPointsClient {updateCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  rewards <- getAllRewards
  responses <- liftIO $ mapConcurrently (go updateCustomReward env id) rewards
  all (== True) <$> mapM handleResponse responses
  where
    go call env id (rewardTitle, rewardId) = do
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
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  [(Text, Effect, Int)] ->
  Eff effs Bool
putAndEnableRewards rewards = do
  rewards' <- identifyNewRewards rewards
  TwitchChannelPointsClient {createCustomReward, updateCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  res <- liftIO (mapConcurrently (createOrEnable createCustomReward updateCustomReward env id) rewards')
  success <- all (== True) <$> (mapM storeCustomRewardCreation . join $ res)
  synchronizeRewardList rewards
  return success
  where
    identifyNewRewards effects = do
      let rewards' = Map.fromList . map (\(title, _, _) -> (title, "")) $ effects
      backendRewards <- Map.fromList <$> getAllRewards
      let allRewards = Map.union backendRewards rewards'
      return $
        map
          ( \(title, eff, cost) ->
              ( title,
                eff,
                cost,
                case Map.lookup title allRewards of
                  Just "" -> Nothing
                  m -> m
              )
          )
          effects
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

synchronizeRewardList ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  [(Text, Effect, Int)] ->
  Eff effs ()
synchronizeRewardList rewards = do
  effs <- Map.fromList <$> getAllRewards
  let updatedTwitchIds =
        map
          ( \(title, eff, _) ->
              case Map.lookup title effs of
                Nothing -> (title, ("", eff))
                Just twitchId -> (title, (twitchId, eff))
          )
          rewards
  modify @TwitchControllerState $ \s -> s & events .~ Map.fromList updatedTwitchIds

-- | Changes the custom reward by adjusting the price for the given reward id.
changeCustomReward ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  Text ->
  Int ->
  Eff effs Bool
changeCustomReward rewardId cost =
  changeCustomReward' rewardId def {updatecustomrewardbodyCost = Just cost}

-- | changeCustomReward' changes the reward identified by the rewardId using
-- the given UpdateCustomRewardBody.
changeCustomReward' ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
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

storeCustomRewardCreation ::
  (Members '[State TwitchControllerState, Logger] effs) =>
  Either Text (Text, (Text, Effect)) ->
  Eff effs Bool
storeCustomRewardCreation (Right (title, (id, eff))) =
  modify @TwitchControllerState (& events %~ Map.insert title (id, eff)) >> return True
storeCustomRewardCreation (Left err) = logError err >> return False

-- | deleteAllRewards deletes all custom channel points rewards managed by this
-- client-id (application).
deleteAllRewards ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  Eff effs Bool
deleteAllRewards = do
  TwitchChannelPointsClient {deleteCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  effs <- getAllRewards
  res <- liftIO $ mapConcurrently (go deleteCustomReward env id) effs
  foldrM resolveResult True res
  where
    go call env id (_, effId) = runClientM (call id effId) env
    resolveResult (Left err) _ = (logError . pack . show $ err) >> return False
    resolveResult _ False = return False
    resolveResult _ _ = return True

-- | getAllRewards returns all rewardids registered for this channel managed by
-- this client-id (application).
getAllRewards ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  Eff effs [(Text, Text)]
getAllRewards = do
  TwitchChannelPointsClient {getCustomRewards} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  liftIO (runClientM (getCustomRewards id Nothing (Just True)) env) >>= \case
    Left err -> (logError . pack . show $ err) >> return []
    Right (DataResponse res) -> return $ map (\r -> (getcustomrewardsdataTitle r, getcustomrewardsdataId r)) res
