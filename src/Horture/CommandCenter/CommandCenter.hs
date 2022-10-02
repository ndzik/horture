{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Horture.CommandCenter.CommandCenter
  ( runCommandCenter,
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Concurrent (forkOS)
import Control.Concurrent.Chan.Synchronous
import Control.Monad.Except
import Data.Default
import Data.List (intercalate)
import Graphics.Vty hiding (Event)
import Graphics.X11 (Window)
import Horture
import Horture.Command
import Horture.CommandCenter.State
import Horture.Event
import Horture.Loader (loadDirectory)
import Numeric (showHex)
import Run
import System.Directory

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
            vBox [withBorderStyle unicode $
              borderWithLabel
                (str "Log")
                runningLogUI,
                withBorderStyle unicode $
                  borderWithLabel (str "Metainformation")
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

runningLogUI :: Widget Name
runningLogUI = center (str "No logs available")

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

appEvent :: BrickEvent Name e -> EventM Name CommandCenterState ()
appEvent (VtyEvent (EvKey (KChar 'j') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'k') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'h') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'l') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'i') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'g') [])) = grabHorture
appEvent (VtyEvent (EvKey (KChar 'q') [])) = stopHorture
appEvent (VtyEvent (EvKey KEsc [])) = stopApplication
appEvent _ = return ()

stopApplication :: EventM Name CommandCenterState ()
stopApplication = do
  gets _ccEventChan >>= \case
    Nothing -> halt
    Just chan -> writeExit chan >> halt

stopHorture :: EventM Name CommandCenterState ()
stopHorture =
  gets _ccEventChan >>= mapM_ writeExit
    >> modify
      ( \ccs ->
          ccs
            { _ccEventChan = Nothing,
              _ccCapturedWin = Nothing
            }
      )

writeExit :: Chan Event -> EventM Name CommandCenterState ()
writeExit chan = liftIO $ writeChan chan (EventCommand Exit)

grabHorture :: EventM Name CommandCenterState ()
grabHorture = do
  evChan <- liftIO $ newChan @Event
  liftIO x11UserGrabWindow >>= \case
    Nothing -> return ()
    Just res@(_, w) -> do
      void . liftIO . forkOS $ run evChan w
      modify $ \ccs ->
        ccs
          { _ccEventChan = Just evChan,
            _ccCapturedWin = Just res
          }

app :: App CommandCenterState e Name
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

runCommandCenter :: IO ()
runCommandCenter = do
  let gifDirectory = "./gifs"
  gifs <- makeAbsolute gifDirectory >>= loadDirectory
  void $ defaultMain app def {_ccGifs = gifs}
