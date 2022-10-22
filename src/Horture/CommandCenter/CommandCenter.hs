{-# LANGUAGE LambdaCase #-}
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
import Data.Text (Text)
import Graphics.Vty hiding (Config, Event)
import Graphics.X11 (Window)
import Horture
import Horture.Command
import Horture.CommandCenter.Event
import Horture.CommandCenter.State
import Horture.Config
import Horture.Event
import Horture.EventSource.Local
import Horture.Loader
import Numeric (showHex)
import Run
import System.Directory
import System.Exit (exitFailure)

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
runningLogUI log = padBottom Max . vBox . map txtWrap $ log

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
            "<esc>: Exit horture"
          ]
    )

appEvent :: BrickEvent Name CommandCenterEvent -> EventM Name CommandCenterState ()
appEvent (VtyEvent (EvKey (KChar 'j') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'k') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'h') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'l') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'i') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'g') [])) = grabHorture
appEvent (VtyEvent (EvKey (KChar 'q') [])) = stopHorture
appEvent (VtyEvent (EvKey KEsc [])) = stopApplication
appEvent (AppEvent e) = handleCCEvent e
appEvent _ = return ()

handleCCEvent :: CommandCenterEvent -> EventM Name CommandCenterState ()
handleCCEvent (CCLog msg) = modify (\cs -> cs {_ccLog = msg : _ccLog cs})

stopApplication :: EventM Name CommandCenterState ()
stopApplication = do
  gets _ccEventChan >>= \case
    Nothing -> halt
    Just chan -> writeExit chan >> halt

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

runCommandCenter :: Config -> IO ()
runCommandCenter (Config _ _ _ _ dir) = do
  gifs <- makeAbsolute dir >>= loadDirectory
  preloadedGifs <-
    runPreloader (PLC dir) loadGifsInMemory >>= \case
      Left err -> print err >> exitFailure
      Right plg -> return plg
  appChan <- newBChan 10
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
          _brickEventChan = Just appChan
        }
