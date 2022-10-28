{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Horture.CommandCenter.CommandCenter
  ( runCommandCenter,
  )
where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center
import Brick.Widgets.List
import qualified Colog
import Control.Concurrent (ThreadId, forkIO, forkOS, killThread)
import Control.Concurrent.Chan.Synchronous
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Data.Default
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Graphics.Vty hiding (Config, Event)
import Graphics.X11 (Window)
import Horture
import Horture.Command
import Horture.CommandCenter.Event
import Horture.CommandCenter.State
import Horture.Config
import Horture.Effect
import Horture.Event
import Horture.EventSource.Controller hiding (logError, logInfo, logWarn)
import Horture.EventSource.Local
import Horture.EventSource.WebSocketClient
import Horture.Loader
import qualified Horture.Logging as HL
import Horture.Object
import Linear.V3
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS
import Network.WebSockets (runClient)
import Numeric (showHex)
import Run
import Servant.Client (mkClientEnv, runClientM)
import Servant.Client.Core.BaseUrl
import System.Directory
import System.Exit (exitFailure)
import Text.Wrap
import Twitch.Rest
import Twitch.Rest.DataResponse
import Twitch.Rest.Types
import Wuss (runSecureClient)

data Name
  = Main
  | AssetList
  deriving (Ord, Show, Eq)

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
  [ vBox
      [ vLimit 4 $
          withBorderStyle unicode $
            borderWithLabel
              (str "Horture CommandCenter")
              (currentCaptureUI . _ccCapturedWin $ cs),
        hBox
          [ withBorderStyle unicode $
              borderWithLabel
                (str "Assets")
                (availableAssetsUI . _ccGifs $ cs),
            vBox
              [ withBorderStyle unicode $
                  borderWithLabel
                    (str "Log")
                    (runningLogUI . _ccLog $ cs),
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

currentCaptureUI :: Maybe (String, Window) -> Widget Name
currentCaptureUI Nothing = center (str "No window is captured")
currentCaptureUI (Just (n, w)) = center (str . unlines $ ["Capturing: " <> n, "WinID: 0x" <> showHex w ""])

instance Splittable [] where
  splitAt n ls = (take n ls, drop n ls)

availableAssetsUI :: [FilePath] -> Widget Name
availableAssetsUI fs = renderList (\_selected el -> str el) False $ list AssetList fs 1

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

appEvent :: BrickEvent Name CommandCenterEvent -> EventM Name CommandCenterState ()
appEvent (VtyEvent (EvKey (KChar 'j') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'k') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'h') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'l') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'i') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'r') [])) = gets _ccControllerChans >>= refreshEventSource
appEvent (VtyEvent (EvKey (KChar 'g') [])) = grabHorture
appEvent (VtyEvent (EvKey (KChar 'q') [])) = stopHorture
appEvent (VtyEvent (EvKey KEsc [])) = stopApplication
appEvent (AppEvent e) = handleCCEvent e
appEvent _ = return ()

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
            _ccTIDsToClean = []
          }
    )

writeExit :: Chan Event -> EventM Name CommandCenterState ()
writeExit chan = liftIO $ writeChan chan (EventCommand Exit)

-- | Refresh the registered events on the connected eventsource, if any is
-- connected.
refreshEventSource ::
  Maybe (Chan EventControllerInput, Chan EventControllerResponse) ->
  EventM Name CommandCenterState ()
refreshEventSource Nothing = logInfo "No EventSource controller available"
refreshEventSource (Just pipe) = do
  writeAndHandleResponse pipe InputPurgeAll

  gifs <- gets _ccPreloadedGifs
  let gifEffs = map (\(fp, _) -> AddGif fp Forever (V3 0 0 0) []) gifs
      shaderEffs = map (AddShaderEffect Forever) [Barrel, Blur, Stitch, Flashbang]
      allEffs = gifEffs ++ [AddScreenBehaviour Forever []] ++ shaderEffs
  mapM_
    (\eff -> writeAndHandleResponse pipe (InputEnable (toTitle eff, eff)))
    allEffs

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

  plg <- gets _ccPreloadedGifs
  hurl <- gets _ccHortureUrl
  brickChan <-
    gets (^. ccBrickEventChan) >>= \case
      Nothing -> throwM InvalidBrickConfiguration
      Just brickChan -> pure brickChan

  logChan <- liftIO $ newChan @Text
  evChan <- liftIO $ newChan @Event
  res@(_, w) <-
    liftIO x11UserGrabWindow >>= \case
      Nothing -> throwM UserAvoidedWindowSelection
      Just res -> pure res

  -- Event source thread.
  evSourceTID <- spawnEventSource hurl evChan logChan
  -- Horture rendering thread. Does not have to be externally killed, because
  -- we will try to end it cooperatively by issuing an external exit command.
  void . liftIO . forkOS $ run plg (Just logChan) evChan w
  -- Logging relay thread `Renderer` -> `Frontend`.
  logSourceTID <- liftIO . forkIO . forever $ pipeToBrickChan logChan brickChan CCLog
  -- Cache ThreadIDs which can to be killed externally.
  ccTIDsToClean .= [evSourceTID, logSourceTID]
  modify $ \ccs ->
    ccs
      { _ccEventChan = Just evChan,
        _ccCapturedWin = Just res
      }

spawnEventSource :: Maybe BaseUrl -> Chan Event -> Chan Text -> EventM Name CommandCenterState ThreadId
spawnEventSource Nothing evChan _ = do
  timeout <- gets (^. ccTimeout)
  gifs <- gets (^. ccGifs)
  liftIO . forkIO $ hortureLocalEventSource timeout evChan gifs
spawnEventSource (Just (BaseUrl scheme host port path)) evChan logChan = do
  registeredEffs <- gets (^. ccRegisteredEffects)
  uid <- gets (^. ccUserId)
  case scheme of
    Https ->
      liftIO . forkIO $
        runSecureClient
          host
          (fromIntegral port)
          path
          (hortureWSStaticClientApp uid evChan registeredEffs)
          `catch` \(e :: SomeException) -> do
            logError . pack . show $ e
    Http ->
      liftIO . forkIO $
        runClient
          host
          port
          path
          (hortureWSStaticClientApp uid evChan registeredEffs)
          `catch` \(e :: SomeException) -> do
            logError . pack . show $ e
  where
    logError = HL.withColog Colog.Error (logActionChan logChan)
    logActionChan :: Chan Text -> Colog.LogAction IO Text
    logActionChan chan = Colog.LogAction $ \msg -> writeChan chan msg

app :: App CommandCenterState CommandCenterEvent Name
app =
  App
    { appDraw = drawUI,
      appStartEvent = prepareEnvironment,
      appHandleEvent = (`catch` handleCCExceptions) . appEvent,
      appAttrMap = const $ attrMap defAttr [],
      appChooseCursor = neverShowCursor
    }

handleCCExceptions :: CCException -> EventM Name CommandCenterState ()
handleCCExceptions InvalidBrickConfiguration = logError "InvalidBrickConfiguration"
handleCCExceptions UserAvoidedWindowSelection = logWarn "User avoided window selection"
handleCCExceptions AlreadyCapturingWindow = logWarn "Already capturing application, stop your current capture first"
handleCCExceptions EventSourceUnavailable = logError "Source of horture events not reachable"

prepareEnvironment :: EventM Name CommandCenterState ()
prepareEnvironment = return ()

runCommandCenter :: Bool -> Config -> IO ()
runCommandCenter mockMode (Config cid _ _ helixApi _ mauth wsEndpoint dir delay) = do
  gifs <- makeAbsolute dir >>= loadDirectory
  preloadedGifs <-
    runPreloader (PLC dir) loadGifsInMemory >>= \case
      Left err -> print err >> exitFailure
      Right plg -> return plg
  appChan <- newBChan 10
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
        { _ccGifs = gifs,
          _ccPreloadedGifs = preloadedGifs,
          _ccHortureUrl = if mockMode then Nothing else wsEndpoint,
          _ccUserId = uid,
          _ccControllerChans = controllerChans,
          _ccBrickEventChan = Just appChan,
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
