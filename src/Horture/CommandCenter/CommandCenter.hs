{-# LANGUAGE NamedFieldPuns #-}

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
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Lens
import Control.Monad (forever, void)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.RingBuffer as RingBuffer
import Data.Text (Text, pack)
import Graphics.Vty hiding (Config, Event)
import Graphics.Vty.Platform.Unix (mkVty)
import Horture.Backend as Backend
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
              hBox
                [ withBorderStyle unicode $
                    borderWithLabel
                      (str "Horture CommandCenter")
                      (currentCaptureUI . _ccCapturedWin $ cs),
                  setAvailableSize (7, 4) $
                    withBorderStyle unicode $
                      borderWithLabel
                        (str "FPS")
                        (center . fpsUI . _ccCurrentFPS $ cs)
                ],
            hBox
              [ let mkAttr =
                      if selectedName == AssetPort
                        then withAttr focused
                        else withAttr unfocused
                 in mkAttr $
                      withBorderStyle unicode $
                        borderWithLabel
                          (str "Assets")
                          (availableAssetsUI . _ccImagesList $ cs),
                vBox
                  [ let mkAttr =
                          if selectedName == LogPort
                            then withAttr focused
                            else withAttr unfocused
                     in mkAttr $
                          withBorderStyle unicode $
                            borderWithLabel
                              (str "Log")
                              (viewport LogPort Vertical . vLimitPercent 100 . runningLogUI . _ccLogList $ cs),
                    withBorderStyle unicode $
                      borderWithLabel
                        (str "Metainformation")
                        metainfoUI
                  ]
              ],
            vLimit 4 $
              withBorderStyle unicode $
                borderWithLabel
                  (str "Hotkeys")
                  hotkeyUI
          ]
      ]

currentCaptureUI :: Maybe String -> Widget Name
currentCaptureUI Nothing = center (str "No window is captured")
currentCaptureUI (Just s) = center (str s)

fpsUI :: Float -> Widget Name
fpsUI = str . show

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
    ( str
        $ unlines
          . map (intercalate " | ")
        $ [ [ "<g>: Select window to capture",
              "<q>: Stop window capture",
              "<r>: Refresh EventSource",
              "<d>: Disable all events"
            ],
            [ "<e>: Enable all events",
              "<p>: Purge EventSource",
              "<s>: Toggle EventSource",
              "<esc>: Exit horture"
            ]
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
    AssetPort -> ccImagesList %= listMoveDown
    _otherwise -> return ()
appEvent (VtyEvent (EvKey (KChar 'k') [])) = do
  gets (^. ccCursorLocationName) >>= \case
    LogPort -> vScrollBy scrollLogPort 1
    AssetPort -> ccImagesList %= listMoveUp
    _otherwise -> return ()
appEvent (VtyEvent (EvKey (KChar 's') [])) = gets _ccEventSourceEnabled >>= toggleEventSource
appEvent (VtyEvent (EvKey (KChar 'h') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'l') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'd') [])) = gets _ccControllerChans >>= disableEventsOnEventSource
appEvent (VtyEvent (EvKey (KChar 'e') [])) = gets _ccControllerChans >>= enableEventsOnEventSource
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
handleCCEvent (CCLog msg) = constructLogFromBuffer msg
handleCCEvent (CCFrameUpdate fps) = ccCurrentFPS .= fps

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

disableEventsOnEventSource ::
  Maybe (Chan EventControllerInput, Chan EventControllerResponse) ->
  EventM Name CommandCenterState ()
disableEventsOnEventSource Nothing = logInfo "No EventSource controller available"
disableEventsOnEventSource (Just pipe) = writeAndHandleResponse pipe InputDisableAll

enableEventsOnEventSource ::
  Maybe (Chan EventControllerInput, Chan EventControllerResponse) ->
  EventM Name CommandCenterState ()
enableEventsOnEventSource Nothing = logInfo "No EventSource controller available"
enableEventsOnEventSource (Just pipe) = do
  writeAndHandleResponse pipe InputEnableAll
  writeAndHandleResponse pipe InputListEvents

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
  allEffs <- deriveBaseEventsCC
  writeAndHandleResponse pipe InputDisableAll
  writeAndHandleResponse pipe . InputEnable $ allEffs
  writeAndHandleResponse pipe InputEnableAll
  writeAndHandleResponse pipe InputListEvents

deriveBaseEventsCC :: EventM Name CommandCenterState [(Text, Effect, Int)]
deriveBaseEventsCC = deriveBaseEvents <$> gets (^. ccEventBaseCost)

deriveBaseEvents :: Int -> [(Text, Effect, Int)]
deriveBaseEvents baseCost = do
  let shaderEffs = map (\v -> AddShaderEffect Forever v []) . enumFrom $ minBound
      behaviourEffs = map (AddScreenBehaviour Forever . (: []) . flip Behaviour (\_ _ o -> o)) . enumFrom $ minBound
      counterEffs = [RemoveScreenBehaviour 0, RemoveShaderEffect 0]
      allEffs =
        behaviourEffs
          ++ [AddAsset "" Forever (V3 0 0 0) [], AddScreenBehaviour Forever [], AddRapidFire []]
          ++ shaderEffs
          ++ counterEffs
   in map (\eff -> (toTitle eff, eff, baseCost * effectToCost eff)) allEffs

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
logActionCC = Colog.LogAction constructLogFromBuffer

constructLogFromBuffer :: Text -> EventM Name CommandCenterState ()
constructLogFromBuffer msg = do
  log <-
    gets (^. ccLog) >>= \rb -> do
      liftIO $ RingBuffer.append msg rb
      liftIO $ RingBuffer.toList rb
  ccLogList .= log

data CCException
  = InvalidBrickConfiguration
  | UserAvoidedWindowSelection
  | AlreadyCapturingWindow
  | EventSourceUnavailable
  | EventControllerUnavailable
  deriving (Show)

instance Exception CCException

grabHorture :: EventM Name CommandCenterState ()
grabHorture = do
  gets (^. ccCapturedWin) >>= \case
    Nothing -> return ()
    Just _ -> throwM AlreadyCapturingWindow

  pli <- gets _ccPreloadedImages
  pls <- gets _ccPreloadedSounds
  hurl <- gets _ccHortureUrl

  brickChan <- gets (^. ccBrickEventChan)
  logChan <- liftIO $ newChan @Text
  evChan <- liftIO $ newChan @Event
  frameCounter <- gets (^. ccFrameCounter)

  -- Event source thread.
  evSourceTID <- spawnEventSource hurl evChan logChan brickChan
  -- Horture rendering thread. Does not have to be externally killed, because
  -- we will try to end it cooperatively by issuing an external exit command.
  mv <- liftIO newEmptyMVar
  mDefaultFont <- gets (^. ccDefaultFont)
  let env =
        HortureInitializerEnvironment
          { _hortureInitializerEnvironmentLogChan = logChan,
            _hortureInitializerEnvironmentGrabbedWin = mv,
            _hortureInitializerEnvironmentDefaultFont = mDefaultFont
          }
      logError = HL.withColog Colog.Error (logActionChan logChan)
  void . liftIO . forkOS $ do
    let startScene =
          def
            { _screen = def,
              _shaders = []
            }
        action = Backend.initialize @'Channel startScene pli pls frameCounter (Just logChan) evChan
    runHortureInitializer env action >>= \case
      Left err -> logError . pack . show $ err
      Right _ -> return ()
  -- Logging relay thread `Renderer` -> `Frontend`.
  logSourceTID <- liftIO . forkIO . forever $ pipeToBrickChan logChan brickChan CCLog
  -- Cache ThreadIDs which can be killed externally.
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

spawnEventSource :: Maybe BaseUrl -> Chan Event -> Chan Text -> BChan CommandCenterEvent -> EventM Name CommandCenterState ThreadId
spawnEventSource Nothing evChan _ _ = do
  timeout <- gets (^. ccTimeout)
  images <- gets (^. ccImages)
  enabledTVar <- fetchOrCreateEventSourceTVar
  liftIO . forkIO $ hortureLocalEventSource timeout evChan images enabledTVar
spawnEventSource (Just (BaseUrl schema host port path)) evChan logChan appChan = do
  runc <- case schema of
    Https -> return runSecureClient
    Http -> return $ \h p -> runClient h (fromIntegral p)
  (ic, rc) <-
    gets (^. ccControllerChans) >>= \case
      Just r -> return r
      Nothing -> throwM EventControllerUnavailable
  env <- StaticEffectRandomizerEnv <$> gets (^. ccRegisteredEffects) <*> gets (^. ccImages)
  uid <- gets (^. ccUserId)
  ccChan <- liftIO $ newChan @CommandCenterEvent
  enabledTVar <- fetchOrCreateEventSourceTVar
  baseEffects <- deriveBaseEventsCC
  let app = hortureWSStaticClientApp baseEffects uid evChan ccChan ic rc env enabledTVar
      run = runc host (fromIntegral port) path app
      action =
        let handler :: ConnectionException -> IO ()
            handler e = do
              logErrorColog logChan . pack . show $ e
              threadDelay oneSec
         in run `catch` (\e -> handler e >> action)
  liftIO . forkIO $ do
    bracket
      (forkIO . forever $ pipeToBrickChan ccChan appChan id)
      killThread
      (const action)

logErrorColog :: Chan Text -> Text -> IO ()
logErrorColog logChan = HL.withColog Colog.Error (logActionChan logChan)

logActionChan :: Chan Text -> Colog.LogAction IO Text
logActionChan chan = Colog.LogAction $ \msg -> writeChan chan msg

oneSec :: Int
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
handleCCExceptions EventControllerUnavailable = logError "Communication channels for event controller unavailable"

prepareEnvironment :: EventM Name CommandCenterState ()
prepareEnvironment = return ()

imagesDir :: String
imagesDir = "/images"

soundsDir :: String
soundsDir = "/sounds"

runDebugCenter :: Maybe Config -> IO ()
runDebugCenter mcfg = do
  let buildVty = mkVty defaultConfig
  appChan <- newBChan 10
  initialVty <- buildVty
  let dir = Horture.Config.assetDirectory def
  images <- makeAbsolute (dir <> imagesDir) >>= loadDirectory
  preloadedImages <-
    runPreloader (PLC $ dir <> imagesDir) loadAssetsInMemory >>= \case
      Left _ -> pure []
      Right pli -> pure pli
  preloadedSounds <-
    runPreloader (PLC $ dir <> soundsDir) loadAssetsInMemory >>= \case
      Left _ -> pure []
      Right pls -> pure pls
  mFont <- case mcfg of
    Just cfg -> return $ Horture.Config.mDefaultFont cfg
    Nothing -> return Nothing
  logBuf <- liftIO $ RingBuffer.new 200
  frameCounter <- liftIO $ newTVarIO 0
  void $ fpsTicker (500 * 1000) frameCounter appChan
  void
    $ customMain
      initialVty
      buildVty
      (Just appChan)
      app
    $ CCState
      { _ccImages = images,
        _ccImagesList = list AssetPort images 1,
        _ccPreloadedImages = preloadedImages,
        _ccPreloadedSounds = preloadedSounds,
        _ccDefaultFont = mFont,
        _ccHortureUrl = Nothing,
        _ccUserId = "some_user_id",
        _ccControllerChans = Nothing,
        _ccLog = logBuf,
        _ccBrickEventChan = appChan,
        _ccEventBaseCost = 10,
        _ccTimeout = 1 * 1000 * 1000,
        _ccEventChan = Nothing,
        _ccCapturedWin = Nothing,
        _ccLogList = [],
        _ccTIDsToClean = [],
        _ccCursorLocationName = LogPort,
        _ccRegisteredEffects = Map.empty,
        _ccFrameCounter = frameCounter,
        _ccEventSourceEnabled = Nothing,
        _ccCurrentFPS = 0
      }

runCommandCenter :: Bool -> Config -> IO ()
runCommandCenter mockMode (Config cid _ _ helixApi _ mauth wsEndpoint baseC dir delay mDefaultFont) = do
  images <- makeAbsolute (dir <> imagesDir) >>= loadDirectory
  appChan <- newBChan 256
  preloadedImages <-
    runPreloader (PLC $ dir <> imagesDir) loadAssetsInMemory >>= \case
      Left err -> print err >> exitFailure
      Right pli -> pure pli
  preloadedSounds <-
    runPreloader (PLC $ dir <> soundsDir) loadAssetsInMemory >>= \case
      Left err -> print err >> exitFailure
      Right pls -> pure pls
  (controllerChans, uid) <-
    if mockMode
      then return (Nothing, "")
      else do
        auth <- case mauth of
          Just auth -> return auth
          Nothing -> error "No AuthorizationToken available, authorize Horture first"
        uid <- fetchUserId helixApi cid auth
        cs <- spawnTwitchEventController baseC helixApi uid cid auth appChan
        return (Just cs, uid)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  logBuf <- liftIO $ RingBuffer.new 200
  frameCounter <- liftIO $ newTVarIO 0
  void $ fpsTicker (500 * 1000) frameCounter appChan
  void
    $ customMain
      initialVty
      buildVty
      (Just appChan)
      app
    $ CCState
      { _ccImages = images,
        _ccImagesList = list AssetPort images 1,
        _ccPreloadedImages = preloadedImages,
        _ccPreloadedSounds = preloadedSounds,
        _ccHortureUrl = if mockMode then Nothing else wsEndpoint,
        _ccUserId = uid,
        _ccLog = logBuf,
        _ccDefaultFont = mDefaultFont,
        _ccControllerChans = controllerChans,
        _ccBrickEventChan = appChan,
        _ccEventBaseCost = baseC,
        _ccTimeout = delay,
        _ccEventChan = Nothing,
        _ccCapturedWin = Nothing,
        _ccLogList = [],
        _ccTIDsToClean = [],
        _ccCursorLocationName = LogPort,
        _ccRegisteredEffects = Map.empty,
        _ccFrameCounter = frameCounter,
        _ccEventSourceEnabled = Nothing,
        _ccCurrentFPS = 0
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
  Int ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  BChan CommandCenterEvent ->
  IO (Chan EventControllerInput, Chan EventControllerResponse)
spawnTwitchEventController baseCost helixApi uid cid auth appChan = do
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
            (Map.fromList . map (\(a, b, c) -> (a, (b, c))) $ deriveBaseEvents baseCost)
            (TCS channelPointsClient uid clientEnv)
            logChan
            controllerInputChan
            controllerResponseChan
      )
  return (controllerInputChan, controllerResponseChan)

fpsTicker :: Int -> TVar Int -> BChan CommandCenterEvent -> IO ThreadId
fpsTicker microSecondsDelay fpsTVar bchan = do
  let loop lastFrameCount = do
        newFrameCount <- readTVarIO fpsTVar
        let numOfFrames = fromIntegral $ newFrameCount - lastFrameCount
            fps = numOfFrames / (fromIntegral microSecondsDelay / (1000 * 1000))
        writeBChan bchan $ CCFrameUpdate fps
        threadDelay microSecondsDelay
        loop newFrameCount
  forkIO $ loop 0

pipeToBrickChan :: (Show a, Show b) => Chan a -> BChan b -> (a -> b) -> IO ()
pipeToBrickChan chan bchan toBchan = do
  -- It is required to use a temporary variable here, otherwise laziness might
  -- lead to a deadlock, because readChan is never evaluated and thus the
  -- synchronization between the synchronous channels is not triggered.
  tmp <- readChan chan
  writeBChan bchan . toBchan $ tmp
