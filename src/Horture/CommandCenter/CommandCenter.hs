{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Concurrent (forkIO, forkOS, killThread)
import Control.Concurrent.Chan.Synchronous
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
import Horture.EventSource.Controller
import Horture.EventSource.Local
import Horture.Loader
import Horture.Object
import Linear.V3
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS
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

data Name
  = Main
  | AssetList
  deriving (Ord, Show, Eq)

-- UI should be:
--

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
terminateEventSource (Just (ic, rc)) =
  liftIO (writeChan ic InputTerminate)
    >> liftIO (readChan rc) >>= handleEventControllerResponse

stopHorture :: EventM Name CommandCenterState ()
stopHorture = do
  gets _ccEventChan >>= mapM_ writeExit
  gets _ccTIDsToClean >>= liftIO . mapM_ killThread
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
refreshEventSource Nothing = logInfo' "No EventSource controller available"
refreshEventSource (Just (ic, rc)) = do
  liftIO (writeChan ic InputPurgeAll) >> liftIO (readChan rc) >>= handleEventControllerResponse
  gifs <- gets _ccPreloadedGifs
  let gifEffs = map (\(fp, _) -> AddGif fp Forever (V3 0 0 0) []) gifs
      shaderEffs = map (AddShaderEffect Forever) [Barrel, Blur, Stitch, Flashbang]
      allEffs = gifEffs ++ [AddScreenBehaviour Forever []] ++ shaderEffs
  mapM_
    ( \eff ->
        do
          liftIO (writeChan ic (InputEnable (toTitle eff, eff)))
          >> liftIO (readChan rc) >>= handleEventControllerResponse
    )
    allEffs
  liftIO (writeChan ic InputListEvents) >> liftIO (readChan rc) >>= handleEventControllerResponse

handleEventControllerResponse :: EventControllerResponse -> EventM Name CommandCenterState ()
handleEventControllerResponse (ListEvents effs) = do
  logInfo' $ "ListingEventSource: " <> (pack . show $ effs)
  modify (\cs -> cs {_ccRegisteredEffects = Map.fromList effs})
handleEventControllerResponse (Enable _itWorked) = return ()
handleEventControllerResponse (PurgeAll _itWorked) = return ()

logInfo' :: Text -> EventM Name CommandCenterState ()
logInfo' msg = handleCCEvent (CCLog msg)

grabHorture :: EventM Name CommandCenterState ()
grabHorture = do
  brickChanM <- gets _brickEventChan
  logChan <- liftIO $ newChan @Text
  evChan <- liftIO $ newChan @Event
  timeout <- gets _ccTimeout
  gifs <- gets _ccGifs
  plg <- gets _ccPreloadedGifs
  liftIO x11UserGrabWindow >>= \case
    Nothing -> return ()
    Just res@(_, w) -> do
      case brickChanM of
        Just brickChan -> do
          evSourceTID <- liftIO . forkIO $ hortureLocalEventSource timeout evChan gifs
          _ <- liftIO . forkOS $ run plg (Just logChan) evChan w
          logSourceTID <- liftIO . forkIO . forever $ do
            readChan logChan >>= writeBChan brickChan . CCLog
          modify (\cs -> cs {_ccTIDsToClean = [evSourceTID, logSourceTID]})
        _otherwise -> return ()
      modify $ \ccs ->
        ccs
          { _ccEventChan = Just evChan,
            _ccCapturedWin = Just res
          }

app :: App CommandCenterState CommandCenterEvent Name
app =
  App
    { appDraw = drawUI,
      appStartEvent = prepareEnvironment,
      appHandleEvent = appEvent,
      appAttrMap = const $ attrMap defAttr [],
      appChooseCursor = neverShowCursor
    }

prepareEnvironment :: EventM Name CommandCenterState ()
prepareEnvironment = return ()

runCommandCenter :: Bool -> Config -> IO ()
runCommandCenter mockMode (Config cid _ helixApi mauth dir) = do
  gifs <- makeAbsolute dir >>= loadDirectory
  preloadedGifs <-
    runPreloader (PLC dir) loadGifsInMemory >>= \case
      Left err -> print err >> exitFailure
      Right plg -> return plg
  appChan <- newBChan 10
  controllerChans <-
    if mockMode
      then return Nothing
      else do
        auth <- case mauth of
          Just auth -> return auth
          Nothing -> error "No AuthorizationToken available, authorize Horture first"

        mgr <-
          newManager =<< case baseUrlScheme helixApi of
            Https -> return tlsManagerSettings
            Http -> return defaultManagerSettings
        let TwitchUsersClient {getUsers} = twitchUsersClient cid (AuthorizationToken auth)
            clientEnv = mkClientEnv mgr helixApi
        res <- runClientM (getUsers Nothing []) clientEnv
        uid <- case res of
          Left err -> do
            print @String "Unabled to get your twitch-id, aborting:"
            error . show $ err
          Right (DataResponse []) -> do
            error "Unabled to get your twitch-id, twitch send unexpected response."
          Right (DataResponse (u : _)) -> return . getuserinformationId $ u

        let channelPointsClient = twitchChannelPointsClient cid (AuthorizationToken auth)
        controllerInputChan <- newChan @EventControllerInput
        controllerResponseChan <- newChan @EventControllerResponse
        void . forkIO $ do
          logChan <- newChan @CommandCenterEvent
          logSourceTID <- forkIO . forever $ do
            readChan logChan >>= writeBChan appChan
          runHortureTwitchEventController
            (TCS channelPointsClient uid clientEnv Map.empty)
            logChan
            controllerInputChan
            controllerResponseChan
          -- Clean up glue-thread.
          killThread logSourceTID
        return . Just $ (controllerInputChan, controllerResponseChan)
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
          _ccControllerChans = controllerChans,
          _brickEventChan = Just appChan
        }
