{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Horture.CommandCenter.CommandCenter
  ( runCommandCenter,
    runDebugCenter,
  )
where

import Brick hiding (cursorLocationName)
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center
import Brick.Widgets.List
import qualified Colog
import Control.Concurrent (ThreadId, forkIO, forkOS, killThread, newEmptyMVar, readMVar, threadDelay)
import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Data.Default
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Graphics.Vty hiding (Config, Event)
import Horture.Backend.X11
import Horture.Command
import Horture.CommandCenter.Event
import Horture.CommandCenter.State
import Horture.Config
import Horture.Effect
import Horture.Event
import Horture.EventSource.Controller hiding (logError, logInfo, logWarn)
import Horture.EventSource.Local
import Horture.EventSource.Random
import Horture.EventSource.WebSocketClient
import Horture.Horture
import Horture.Initializer
import Horture.Loader
import qualified Horture.Logging as HL
import Horture.Object
import Horture.Scene
import Linear.V3
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS
import Network.WebSockets (ConnectionException (..), runClient)
import Servant.Client (mkClientEnv, runClientM)
import Servant.Client.Core.BaseUrl
import System.Directory
import System.Exit (exitFailure)
import Text.Wrap
import Twitch.Rest
import Twitch.Rest.DataResponse
import Twitch.Rest.Types
import Wuss (runSecureClient)

-- | |----------------------------------------|
--   | Captured Window Title + ID             |
--   |----------------------------------------|
--   | Assets           | Running log         |
--   |                  |                     |
--   |                  |---------------------|
--   |                  | MetaInfos           |
--   |                  |                     |
--   |------------------|---------------------|
drawUI :: CommandCenterState -> [Widget Name]
drawUI cs =
  let selectedName = (cs ^. ccCursorLocationName)
   in [ vBox
          [ vLimit 4 $
              withBorderStyle unicode $
                borderWithLabel
                  (str "Horture CommandCenter")
                  (currentCaptureUI . _ccCapturedWin $ cs),
            hBox
              [ let mkAttr =
                      if selectedName == AssetPort
                        then withAttr focused
                        else withAttr unfocused
                 in mkAttr $
                      withBorderStyle unicode $
                        borderWithLabel
                          (str "Assets")
                          (availableAssetsUI . _ccAssetsList $ cs),
                vBox
                  [ let mkAttr =
                          if selectedName == LogPort
                            then withAttr focused
                            else withAttr unfocused
                     in mkAttr $
                          withBorderStyle unicode $
                            borderWithLabel
                              (str "Log")
                              (viewport LogPort Vertical . vLimitPercent 100 . runningLogUI . _ccLog $ cs),
                    withBorderStyle unicode $
                      borderWithLabel
                        (str "Metainformation")
                        metainfoUI
                  ]
              ],
            vLimit 3 $
              withBorderStyle unicode $
                borderWithLabel
                  (str "Hotkeys")
                  hotkeyUI
          ]
      ]

currentCaptureUI :: Maybe String -> Widget Name
currentCaptureUI Nothing = center (str "No window is captured")
currentCaptureUI (Just s) = center (str s)

instance Splittable [] where
  splitAt n ls = (take n ls, drop n ls)

availableAssetsUI :: GenericList Name [] FilePath -> Widget Name
availableAssetsUI =
  renderList
    ( \selected el ->
        if selected
          then withAttr listSelectedAttr (str $ "<" <> el <> ">")
          else withAttr unfocused $ str el
    )
    False

runningLogUI :: [Text] -> Widget Name
runningLogUI [] = center (str "No logs available")
runningLogUI log = padBottom Max . vBox . map (txtWrapWith (defaultWrapSettings {breakLongWords = True})) $ log

metainfoUI :: Widget Name
metainfoUI = center (str "No metainformation available")

hotkeyUI :: Widget Name
hotkeyUI =
  center
    ( str $
        intercalate
          " | "
          [ "<g>: Select window to capture",
            "<q>: Stop window capture",
            "<r>: Refresh EventSource events",
            "<esc>: Exit horture"
          ]
    )

scrollLogPort :: ViewportScroll Name
scrollLogPort = viewportScroll LogPort

appEvent :: BrickEvent Name CommandCenterEvent -> EventM Name CommandCenterState ()
appEvent (VtyEvent (EvKey (KChar '\t') [])) =
  ccCursorLocationName %= \case
    LogPort -> AssetPort
    AssetPort -> LogPort
    _otherwise -> LogPort
appEvent (VtyEvent (EvKey (KChar 'j') [])) = do
  gets (^. ccCursorLocationName) >>= \case
    LogPort -> vScrollBy scrollLogPort (-1)
    AssetPort -> ccAssetsList %= listMoveDown
    _otherwise -> return ()
appEvent (VtyEvent (EvKey (KChar 'k') [])) = do
  gets (^. ccCursorLocationName) >>= \case
    LogPort -> vScrollBy scrollLogPort 1
    AssetPort -> ccAssetsList %= listMoveUp
    _otherwise -> return ()
appEvent (VtyEvent (EvKey (KChar 's') [])) = gets _ccEventSourceEnabled >>= toggleEventSource
appEvent (VtyEvent (EvKey (KChar 'h') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'l') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'p') [])) = gets _ccControllerChans >>= purgeEventSource
appEvent (VtyEvent (EvKey (KChar 'r') [])) = gets _ccControllerChans >>= refreshEventSource
appEvent (VtyEvent (EvKey (KChar 'g') [])) = grabHorture
appEvent (VtyEvent (EvKey (KChar 'q') [])) = stopHorture
appEvent (VtyEvent (EvKey KEsc [])) = stopApplication
appEvent (AppEvent e) = handleCCEvent e
appEvent _else = return ()

toggleEventSource :: Maybe (TVar Bool) -> EventM Name CommandCenterState ()
toggleEventSource Nothing = logWarn "No eventsource is running"
toggleEventSource (Just tv) = do
  liftIO . atomically . modifyTVar tv $ not
  logInfo "EventSource toggled"

handleCCEvent :: CommandCenterEvent -> EventM Name CommandCenterState ()
handleCCEvent (CCLog msg) = modify (\cs -> cs {_ccLog = msg : _ccLog cs})

stopApplication :: EventM Name CommandCenterState ()
stopApplication = do
  gets _ccControllerChans >>= terminateEventSource
  gets _ccEventChan >>= \case
    Nothing -> halt
    Just chan -> writeExit chan >> halt

terminateEventSource ::
  Maybe (Chan EventControllerInput, Chan EventControllerResponse) ->
  EventM Name CommandCenterState ()
terminateEventSource Nothing = return ()
terminateEventSource (Just pipe) = writeAndHandleResponse pipe InputTerminate

stopHorture :: EventM Name CommandCenterState ()
stopHorture = do
  gets (^. ccEventChan) >>= mapM_ writeExit
  gets (^. ccTIDsToClean) >>= liftIO . mapM_ killThread
  modify
    ( \ccs ->
        ccs
          { _ccEventChan = Nothing,
            _ccCapturedWin = Nothing,
            _ccTIDsToClean = [],
            _ccEventSourceEnabled = Nothing
          }
    )

writeExit :: Chan Event -> EventM Name CommandCenterState ()
writeExit chan = liftIO $ writeChan chan (EventCommand Exit)

purgeEventSource ::
  Maybe (Chan EventControllerInput, Chan EventControllerResponse) ->
  EventM Name CommandCenterState ()
purgeEventSource Nothing = logInfo "No EventSource controller available"
purgeEventSource (Just pipe) = writeAndHandleResponse pipe InputPurgeAll

-- | Refresh the registered events on the connected eventsource, if any is
-- connected.
refreshEventSource ::
  Maybe (Chan EventControllerInput, Chan EventControllerResponse) ->
  EventM Name CommandCenterState ()
refreshEventSource Nothing = logInfo "No EventSource controller available"
refreshEventSource (Just pipe) = do
  writeAndHandleResponse pipe InputPurgeAll

  -- assets <- gets (^. ccPreloadedAssets)
  baseCost <- gets (^. ccEventBaseCost)
  let -- assetEffs = map (\(fp, _) -> AddAsset fp Forever (V3 0 0 0) []) assets
      shaderEffs = map (AddShaderEffect Forever) . enumFrom $ minBound
      behaviourEffs = map (AddScreenBehaviour Forever . (: []) . flip Behaviour (\_ _ o -> o)) . enumFrom $ minBound
      allEffs =
        behaviourEffs
          ++ [AddAsset "" Forever (V3 0 0 0) [], AddScreenBehaviour Forever [], AddRapidFire []]
          ++ shaderEffs

  writeAndHandleResponse pipe . InputEnable . map (\eff -> (toTitle eff, eff, baseCost * effectToCost eff)) $ allEffs

  writeAndHandleResponse pipe InputListEvents

writeAndHandleResponse ::
  (Chan EventControllerInput, Chan EventControllerResponse) ->
  EventControllerInput ->
  EventM Name CommandCenterState ()
writeAndHandleResponse (ic, rc) i = do
  liftIO (writeChan ic i)
  liftIO (readChan rc) >>= handleEventControllerResponse

handleEventControllerResponse :: EventControllerResponse -> EventM Name CommandCenterState ()
handleEventControllerResponse (ListEvents effs) = do
  logInfo "Listing event source:"
  mapM_ (logInfo . pack . show) effs
  modify (\cs -> cs {_ccRegisteredEffects = Map.fromList effs})
handleEventControllerResponse (Enable itWorked)
  | itWorked = logInfo "Enabling events on EventSource successful"
  | otherwise = logWarn "Enabling events on EventSource failed"
handleEventControllerResponse (PurgeAll itWorked)
  | itWorked = logInfo "Purging events on EventSource successful"
  | otherwise = logWarn "Purging events on EventSource failed"

logInfo :: Text -> EventM Name CommandCenterState ()
logInfo = HL.withColog Colog.Info logActionCC

logWarn :: Text -> EventM Name CommandCenterState ()
logWarn = HL.withColog Colog.Warning logActionCC

logError :: Text -> EventM Name CommandCenterState ()
logError = HL.withColog Colog.Error logActionCC

logActionCC :: Colog.LogAction (EventM Name CommandCenterState) Text
logActionCC = Colog.LogAction $ \msg -> ccLog %= (msg :)

data CCException
  = InvalidBrickConfiguration
  | UserAvoidedWindowSelection
  | AlreadyCapturingWindow
  | EventSourceUnavailable
  deriving (Show)

instance Exception CCException

grabHorture :: EventM Name CommandCenterState ()
grabHorture = do
  gets (^. ccCapturedWin) >>= \case
    Nothing -> return ()
    Just _ -> throwM AlreadyCapturingWindow

  plg <- gets _ccPreloadedAssets
  hurl <- gets _ccHortureUrl
  brickChan <-
    gets (^. ccBrickEventChan) >>= \case
      Nothing -> throwM InvalidBrickConfiguration
      Just brickChan -> pure brickChan

  logChan <- liftIO $ newChan @Text
  evChan <- liftIO $ newChan @Event

  -- Event source thread.
  evSourceTID <- spawnEventSource hurl evChan logChan
  -- Horture rendering thread. Does not have to be externally killed, because
  -- we will try to end it cooperatively by issuing an external exit command.
  mv <- liftIO newEmptyMVar
  let env =
        HortureInitializerEnvironment
          { _hortureInitializerEnvironmentLogChan = logChan,
            _hortureInitializerEnvironmentGrabbedWin = mv
          }
      logError = HL.withColog Colog.Error (logActionChan logChan)
  void . liftIO . forkOS $ do
    let startScene =
          def
            { _screen = def,
              _shaders = []
            }
        action = initialize @'Channel startScene plg (Just logChan) evChan
    runHortureInitializer env action >>= \case
      Left err -> logError . pack . show $ err
      Right _ -> return ()
  -- Logging relay thread `Renderer` -> `Frontend`.
  logSourceTID <- liftIO . forkIO . forever $ pipeToBrickChan logChan brickChan CCLog
  -- Cache ThreadIDs which can to be killed externally.
  ccTIDsToClean .= [evSourceTID, logSourceTID]

  res <-
    liftIO (readMVar mv) >>= \case
      Nothing -> throwM UserAvoidedWindowSelection
      Just res -> return res
  modify $ \ccs ->
    ccs
      { _ccEventChan = Just evChan,
        _ccCapturedWin = Just res
      }
  where
    logActionChan :: Chan Text -> Colog.LogAction IO Text
    logActionChan chan = Colog.LogAction $ \msg -> writeChan chan msg

-- | fetchOrCreateEventSourceTVar looks up if an EventSourceTVar is already
-- registered. If that is not the case, it creates a default initialized TVar,
-- registers it in the `CommandCenterState` and returns mentioned tvar.
fetchOrCreateEventSourceTVar :: EventM Name CommandCenterState (TVar Bool)
fetchOrCreateEventSourceTVar = do
  tv <-
    gets (^. ccEventSourceEnabled) >>= \case
      Nothing -> liftIO $ newTVarIO True
      Just tv -> return tv
  ccEventSourceEnabled .= Just tv
  return tv

spawnEventSource :: Maybe BaseUrl -> Chan Event -> Chan Text -> EventM Name CommandCenterState ThreadId
spawnEventSource Nothing evChan _ = do
  timeout <- gets (^. ccTimeout)
  assets <- gets (^. ccAssets)
  enabledTVar <- fetchOrCreateEventSourceTVar
  liftIO . forkIO $ hortureLocalEventSource timeout evChan assets enabledTVar
spawnEventSource (Just (BaseUrl scheme host port path)) evChan logChan = do
  registeredEffs <- gets (^. ccRegisteredEffects)
  assetEffs <- gets (^. ccAssets)
  uid <- gets (^. ccUserId)
  enabledTVar <- fetchOrCreateEventSourceTVar
  let env =
        StaticEffectRandomizerEnv
          { _registeredEffects = registeredEffs,
            _assetEffects = assetEffs
          }
  case scheme of
    Https ->
      liftIO . forkIO $ do
        let action =
              runSecureClient
                host
                (fromIntegral port)
                path
                (hortureWSStaticClientApp uid evChan env enabledTVar)
                `catch` \(e :: ConnectionException) -> do
                  logError . pack . show $ e
                  threadDelay oneSec
                  action
        action
    Http ->
      liftIO . forkIO $ do
        let action =
              runClient
                host
                (fromIntegral port)
                path
                (hortureWSStaticClientApp uid evChan env enabledTVar)
                `catch` \(e :: ConnectionException) -> do
                  logError . pack . show $ e
                  threadDelay oneSec
                  action
        action
  where
    logError = HL.withColog Colog.Error (logActionChan logChan)
    logActionChan :: Chan Text -> Colog.LogAction IO Text
    logActionChan chan = Colog.LogAction $ \msg -> writeChan chan msg
    oneSec = 1_000_000

focused :: AttrName
focused = attrName "focused"

unfocused :: AttrName
unfocused = attrName "unfocused"

app :: App CommandCenterState CommandCenterEvent Name
app =
  App
    { appDraw = drawUI,
      appStartEvent = prepareEnvironment,
      appHandleEvent = (`catch` handleCCExceptions) . appEvent,
      appAttrMap =
        const $
          attrMap
            defAttr
            [ (listSelectedAttr, fg blue),
              (focused, fg yellow),
              (unfocused, fg white)
            ],
      appChooseCursor = neverShowCursor
    }

handleCCExceptions :: CCException -> EventM Name CommandCenterState ()
handleCCExceptions InvalidBrickConfiguration = logError "InvalidBrickConfiguration"
handleCCExceptions UserAvoidedWindowSelection = logWarn "User avoided window selection"
handleCCExceptions AlreadyCapturingWindow = logWarn "Already capturing application, stop your current capture first"
handleCCExceptions EventSourceUnavailable = logError "Source of horture events not reachable"

prepareEnvironment :: EventM Name CommandCenterState ()
prepareEnvironment = return ()

runDebugCenter :: IO ()
runDebugCenter = do
  let buildVty = mkVty defaultConfig
  appChan <- newBChan 10
  initialVty <- buildVty
  let dir = Horture.Config.assetDirectory def
  assets <- makeAbsolute dir >>= loadDirectory
  preloadedAssets <-
    runPreloader (PLC dir) loadAssetsInMemory >>= \case
      Left _ -> pure []
      Right plg -> pure plg
  void $
    customMain
      initialVty
      buildVty
      (Just appChan)
      app
      def
        { _ccAssets = assets,
          _ccAssetsList = list AssetPort assets 1,
          _ccPreloadedAssets = preloadedAssets,
          _ccHortureUrl = Nothing,
          _ccUserId = "some_user_id",
          _ccControllerChans = Nothing,
          _ccBrickEventChan = Just appChan,
          _ccEventBaseCost = 10,
          _ccTimeout = 1 * 1000 * 1000
        }

runCommandCenter :: Bool -> Config -> IO ()
runCommandCenter mockMode (Config cid _ _ helixApi _ mauth wsEndpoint baseC dir delay) = do
  assets <- makeAbsolute dir >>= loadDirectory
  appChan <- newBChan 10
  preloadedAssets <-
    runPreloader (PLC dir) loadAssetsInMemory >>= \case
      Left err -> print err >> exitFailure
      Right pla -> pure pla
  (controllerChans, uid) <-
    if mockMode
      then return (Nothing, "")
      else do
        auth <- case mauth of
          Just auth -> return auth
          Nothing -> error "No AuthorizationToken available, authorize Horture first"
        uid <- fetchUserId helixApi cid auth
        cs <- spawnTwitchEventController helixApi uid cid auth appChan
        return (Just cs, uid)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $
    customMain
      initialVty
      buildVty
      (Just appChan)
      app
      def
        { _ccAssets = assets,
          _ccAssetsList = list AssetPort assets 1,
          _ccPreloadedAssets = preloadedAssets,
          _ccHortureUrl = if mockMode then Nothing else wsEndpoint,
          _ccUserId = uid,
          _ccControllerChans = controllerChans,
          _ccBrickEventChan = Just appChan,
          _ccEventBaseCost = baseC,
          _ccTimeout = delay
        }

fetchUserId :: BaseUrl -> Text -> Text -> IO Text
fetchUserId helixApi cid auth = do
  mgr <-
    newManager =<< case baseUrlScheme helixApi of
      Https -> return tlsManagerSettings
      Http -> return defaultManagerSettings
  let TwitchUsersClient {getUsers} = twitchUsersClient cid (AuthorizationToken auth)
      clientEnv = mkClientEnv mgr helixApi
  res <- runClientM (getUsers Nothing []) clientEnv
  case res of
    Left err -> do
      print @String "Unabled to get your twitch-id, aborting:"
      print err
      exitFailure
    Right (DataResponse []) -> do
      print @String "Unabled to get your twitch-id, twitch send unexpected response."
      exitFailure
    Right (DataResponse (u : _)) -> return . getuserinformationId $ u

spawnTwitchEventController ::
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  BChan CommandCenterEvent ->
  IO (Chan EventControllerInput, Chan EventControllerResponse)
spawnTwitchEventController helixApi uid cid auth appChan = do
  mgr <-
    newManager =<< case baseUrlScheme helixApi of
      Https -> return tlsManagerSettings
      Http -> return defaultManagerSettings
  let channelPointsClient = twitchChannelPointsClient cid (AuthorizationToken auth)
      clientEnv = mkClientEnv mgr helixApi
  controllerInputChan <- newChan @EventControllerInput
  controllerResponseChan <- newChan @EventControllerResponse
  void . forkIO $ do
    logChan <- newChan @CommandCenterEvent
    bracket
      (forkIO . forever $ pipeToBrickChan logChan appChan id)
      killThread
      ( const $
          runHortureTwitchEventController
            (TCS channelPointsClient uid clientEnv Map.empty)
            logChan
            controllerInputChan
            controllerResponseChan
      )
  return (controllerInputChan, controllerResponseChan)

pipeToBrickChan :: Chan a -> BChan b -> (a -> b) -> IO ()
pipeToBrickChan chan bchan toBchan = readChan chan >>= writeBChan bchan . toBchan
