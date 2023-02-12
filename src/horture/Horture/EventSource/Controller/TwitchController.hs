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
  ListAllEvents -> gets @TwitchControllerState (Map.toList . (^. events))
  EnableEvents c -> putCustomRewards c
  ChangeEventCost id c -> changeCustomReward id c
  PurgeAllEvents -> deleteAllRewards

-- | putCustomRewards registers the given [(Title, Effect)] pairs as custom
-- channel points rewards on the associated channel.
putCustomRewards ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  [(Text, Effect, Int)] ->
  Eff effs Bool
putCustomRewards rewards = do
  TwitchChannelPointsClient {createCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  res <- liftIO (mapConcurrently (go createCustomReward env id) rewards)
  all (== True) <$> (mapM storeCustomRewardCreation . join $ res)
  where
    go call env id (title, eff, cost) = do
      let body =
            CreateCustomRewardBody
              { createcustomrewardbodyTitle = title,
                createcustomrewardbodyCost = cost,
                createcustomrewardbodyPrompt = Nothing,
                createcustomrewardbodyIsEnabled = Nothing,
                createcustomrewardbodyBackgroundColor = Nothing,
                createcustomrewardbodyIsUserInputRequired = Nothing,
                createcustomrewardbodyIsMaxPerStreamEnabled = Nothing,
                createcustomrewardbodyMaxPerStream = Nothing,
                createcustomrewardbodyIsMaxPerUserPerStreamEnabled = Nothing,
                createcustomrewardbodyMaxPerUserPerStream = Nothing,
                createcustomrewardbodyIsGlobalCooldownEnabled = Nothing,
                createcustomrewardbodyGlobalCooldownSeconds = Nothing,
                createcustomrewardbodyShouldRedemptionsSkipRequestQuee = Nothing
              }
      runClientM (call id body) env >>= \case
        Right (DataResponse crs) -> return $ map (\d -> Right (title, (getcustomrewardsdataId d, eff))) crs
        Left err -> return [Left . pack . show $ err]

changeCustomReward ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  Text ->
  Int ->
  Eff effs Bool
changeCustomReward rewardId cost = do
  TwitchChannelPointsClient {updateCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  go (updateCustomReward id rewardId) env cost
    where go call env cost = do
            let body = UpdateCustomRewardBody
                        {
                updatecustomrewardbodyTitle = Nothing,
                updatecustomrewardbodyCost = Just cost,
                updatecustomrewardbodyPrompt = Nothing,
                updatecustomrewardbodyIsEnabled = Nothing,
                updatecustomrewardbodyBackgroundColor = Nothing,
                updatecustomrewardbodyIsUserInputRequired = Nothing,
                updatecustomrewardbodyIsMaxPerStreamEnabled = Nothing,
                updatecustomrewardbodyMaxPerStream = Nothing,
                updatecustomrewardbodyIsMaxPerUserPerStreamEnabled = Nothing,
                updatecustomrewardbodyMaxPerUserPerStream = Nothing,
                updatecustomrewardbodyIsGlobalCooldownEnabled = Nothing,
                updatecustomrewardbodyGlobalCooldownSeconds = Nothing,
                updatecustomrewardbodyShouldRedemptionsSkipRequestQuee = Nothing
                        }
            liftIO (runClientM (call body) env) >>= \case
              Right _ -> return True
              Left err -> logError (pack . show $ err) >> return False

storeCustomRewardCreation ::
  (Members '[State TwitchControllerState, Logger] effs) =>
  Either Text (Text, (Text, Effect)) ->
  Eff effs Bool
storeCustomRewardCreation (Right (title, (id, eff))) = modify @TwitchControllerState (& events %~ Map.insert title (id, eff)) >> return True
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
    go call env id effId = runClientM (call id effId) env
    resolveResult (Left err) _ = (logError . pack . show $ err) >> return False
    resolveResult _ False = return False
    resolveResult _ _ = return True

-- | getAllRewards returns all rewardids registered for this channel managed by
-- this client-id (application).
getAllRewards ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  Eff effs [Text]
getAllRewards = do
  TwitchChannelPointsClient {getCustomRewards} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  liftIO (runClientM (getCustomRewards id Nothing (Just True)) env) >>= \case
    Left err -> (logError . pack . show $ err) >> return []
    Right (DataResponse res) -> return $ map getcustomrewardsdataId res
