{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Horture.EventSource.Controller.TwitchController
  ( handleTwitchEventController,
    runHortureTwitchEventController,
  )
where

import Colog (Severity (..))
import Control.Concurrent.Chan.Synchronous
import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
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
  Chan (Severity, Text) ->
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
  EnableEvent c -> putCustomReward c
  PurgeAllEvents -> deleteAllRewards

-- | putCustomReward registers the given (Title, Effect) pair as a custom
-- channel points reward on the associated channel.
putCustomReward ::
  (Members '[State TwitchControllerState, Logger] effs, LastMember IO effs) =>
  (Text, Effect) ->
  Eff effs Bool
putCustomReward (title, eff) = do
  TwitchChannelPointsClient {createCustomReward} <- gets @TwitchControllerState (^. client)
  id <- gets @TwitchControllerState (^. broadcasterId)
  env <- gets @TwitchControllerState (^. clientEnv)
  let body =
        CreateCustomRewardBody
          { createcustomrewardbodyTitle = title,
            createcustomrewardbodyCost = 50,
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
  liftIO (runClientM (createCustomReward id body) env) >>= \case
    Right (DataResponse crs) -> mapM_ (storeCustomRewardCreation (title, eff) . getcustomrewardsdataId) crs >> return True
    Left err -> (logError . pack . show $ err) >> return False

storeCustomRewardCreation ::
  (Members '[State TwitchControllerState] effs) =>
  (Text, Effect) ->
  Text ->
  Eff effs ()
storeCustomRewardCreation (title, eff) id = modify @TwitchControllerState (& events %~ Map.insert title (id, eff))

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
  ress <- mapM (\effId -> liftIO (runClientM (deleteCustomReward id effId) env)) effs
  foldrM resolveResult True ress
  where
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
  liftIO (runClientM (getCustomRewards id Nothing Nothing) env) >>= \case
    Left err -> (logError . pack . show $ err) >> return []
    Right (DataResponse res) -> return $ map getcustomrewardsdataId res
