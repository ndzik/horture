{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Horture.CommandCenter.GUI (runCommandCenterUI) where

import Control.Concurrent.Chan.Synchronous (Chan, newChan, writeChan)
import Control.Lens
import Control.Monad (void)
import Data.Default
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Horture.Server
import Horture.Server.Protocol
import Monomer
import Monomer.Core.Themes.BaseTheme (BaseThemeColors (..), baseTheme)

data CCModel = CCModel
  { _mCapturedWin :: Maybe Text,
    _mFPS :: Double,
    _mConn :: Maybe (Chan CCCommand),
    _mAssets :: [FilePath],
    _mSelAsset :: Int,
    _mLogLines :: [Text],
    _mMeta :: Text
  }

data Pane = PaneAssets | PaneLog
  deriving (Eq, Show)

makeLenses 'CCModel

instance Eq CCModel where
  a == b =
    (a ^. mCapturedWin) == (b ^. mCapturedWin)
      && (a ^. mFPS) == (b ^. mFPS)
      && (a ^. mAssets) == (b ^. mAssets)
      && (a ^. mSelAsset) == (b ^. mSelAsset)
      && (a ^. mLogLines) == (b ^. mLogLines)
      && (a ^. mMeta) == (b ^. mMeta)

instance Default CCModel where
  def =
    CCModel
      { _mCapturedWin = Nothing,
        _mConn = Nothing,
        _mFPS = 0,
        _mAssets = [],
        _mSelAsset = 0,
        _mLogLines = [],
        _mMeta = "No metainformation available"
      }

data CCEvent
  = EvTickFPS Double
  | EvAppendLog Text
  | EvConnectionEstablished (Chan CCCommand)
  | EvSetAssets [FilePath]
  | EvStartCapture
  | EvCapturedWindow Text
  | EvStopCapture
  | EvToggleES
  | EvRefreshES
  | EvDisableAll
  | EvEnableAll
  | EvPurgeAll
  | EvExit
  | EvNoop

runCommandCenterUI :: [FilePath] -> IO ()
runCommandCenterUI initialAssets = do
  let cfg =
        [ appWindowTitle "Horture",
          appTheme customTheme,
          appInitEvent (EvSetAssets initialAssets),
          appFontDef "Regular" "./assets/fonts/CaskaydiaMonoNerdFontMono-Regular.ttf",
          appWindowState (MainWindowNormal (1200, 800)),
          appDisposeEvent EvExit
        ]
  startApp def handleEvent buildUI cfg

customTheme :: Theme
customTheme = baseTheme customColormap
  where
    customColormap =
      darkThemeColors
        { inputText = black,
          labelText = black,
          btnBgBasic = mustardLight,
          btnBgHover = mustardBase,
          btnBgFocus = mustardDark,
          btnBgActive = mustardLight,
          btnBgDisabled = gray,
          btnFocusBorder = mustardHighlight
        }

titleSize :: Double
titleSize = 46

headerSize :: Double
headerSize = 20

buildUI :: WidgetEnv CCModel CCEvent -> CCModel -> WidgetNode CCModel CCEvent
buildUI _ m =
  vstack
    [ vstack
        [ hstack
            [ vstack
                [ label "CommandCenter" `styleBasic` [textSize titleSize, textCenter],
                  separatorLine `styleBasic` [paddingH 4]
                ],
              spacer_ [width 128],
              box captureButton,
              spacer,
              box toggleEventSourceButton,
              filler,
              spacer_ [width 32],
              boxShadow . box $
                ( vstack
                    [ label "FPS" `styleBasic` [textSize headerSize, textCenter],
                      separatorLine,
                      spacer,
                      (label (showt (m ^. mFPS))) `styleBasic` [textCenter]
                    ]
                )
                  `styleBasic` [bgColor mustardLight, radius 6, padding 4, width 128]
            ]
            `styleBasic` [padding 8],
          hstack
            [ filler,
              boxShadow $
                (capturedWinW m)
                  `styleBasic` [bgColor mustardLight, radius 6, padding 4],
              filler
            ]
        ],
      hstack
        [ boxShadow $
            box_
              [alignTop]
              ( vstack
                  [ label "Assets" `styleBasic` [textSize headerSize, textCenter],
                    separatorLine `styleBasic` [paddingH 4],
                    spacer,
                    vstack assetList
                      `styleBasic` [padding 8, radius 6]
                  ]
              )
              `styleBasic` [bgColor mustardLight, radius 6, padding 4],
          spacer,
          boxShadow $
            box
              ( vstack
                  [ label "Log" `styleBasic` [textSize headerSize, textCenter],
                    separatorLine `styleBasic` [paddingH 4],
                    spacer,
                    scroll_ [wheelRate 40] (vstack (map (label . id) (m ^. mLogLines)))
                      `styleBasic` [padding 8, radius 6, height 300]
                  ]
              )
              `styleBasic` [bgColor mustardLight, radius 6, padding 4]
        ]
        `styleBasic` [paddingT 4],
      spacer,
      boxShadow . box_ [expandContent] $
        ( vstack
            [ label "Metainformation" `styleBasic` [textSize headerSize, textCenter],
              separatorLine `styleBasic` [paddingH 4],
              spacer,
              box (label (m ^. mMeta))
            ]
            `styleBasic` []
        )
          `styleBasic` [bgColor mustardLight, padding 8, radius 6]
    ]
    `styleBasic` [bgColor mustardBase]
  where
    assetList =
      if null (m ^. mAssets)
        then [label "No assets available"]
        else map mkAssetRow (zip [0 ..] (m ^. mAssets))
    mkAssetRow (i, fp) =
      let isSel = m ^. mSelAsset == i
       in label (T.pack fp)
            `styleBasic` [ padding 4,
                           border 1 (if isSel then green else gray)
                         ]
    capturedWinW mm =
      case mm ^. mCapturedWin of
        Nothing -> label "No window is captured"
        Just t -> label t

    captureButton =
      case m ^. mCapturedWin of
        Nothing ->
          button "Start Capture" EvStartCapture
            `styleBasic` [textSize 12]
        Just _ ->
          button "Stop Capture" EvStopCapture
            `styleBasic` [bgColor softRed, textSize 12]

    toggleEventSourceButton =
      button "Toggle Event Source" EvToggleES
        `styleBasic` [textSize 12]
        `nodeEnabled` (isJust $ m ^. mConn)

mustardBase, mustardLight, mustardDark, mustardHighlight, softRed :: Color
mustardDark = rgbHex "#f2d279"
mustardBase = rgbHex "#f4d88b"
mustardLight = rgbHex "#f8e7b9"
mustardHighlight = rgbHex "#d19f15"
softRed = rgbHex "#f5655b"

handleEvent ::
  WidgetEnv CCModel CCEvent ->
  WidgetNode CCModel CCEvent ->
  CCModel ->
  CCEvent ->
  [EventResponse CCModel CCEvent CCModel CCEvent]
handleEvent _ _ m = \case
  EvSetAssets as ->
    [Model (m & mAssets .~ as & mSelAsset .~ 0)]
  EvTickFPS fps ->
    [Model (m & mFPS .~ fps)]
  EvAppendLog t ->
    [ Model (m & mLogLines %~ (\xs -> take 500 (t : xs))),
      Task (pure EvNoop)
    ]
  EvStartCapture -> [Model m, Producer startCaptureServer]
  EvStopCapture ->
    [ Model (m & mCapturedWin .~ Nothing & mConn .~ Nothing & mFPS .~ 0),
      Task (stopCaptureServer $ m ^. mConn)
    ]
  EvCapturedWindow stitle -> [Model (m & mCapturedWin .~ Just stitle)]
  EvToggleES -> [Model m, Task $ toggleEventStream (m ^. mConn)]
  EvConnectionEstablished chan -> [Model (m & mConn ?~ chan)]
  EvRefreshES -> [Model m]
  EvDisableAll -> [Model m]
  EvEnableAll -> [Model m]
  EvPurgeAll -> [Model m]
  EvNoop -> [Model m]
  EvExit -> [Task $ stopCaptureServer (m ^. mConn)]

toggleEventStream :: Maybe (Chan CCCommand) -> IO CCEvent
toggleEventStream Nothing = pure (EvAppendLog "Event stream channel is not available")
toggleEventStream (Just chan) = do
  writeChan chan CmdToggleEvents
  pure EvNoop

stopCaptureServer :: Maybe (Chan CCCommand) -> IO CCEvent
stopCaptureServer Nothing = do
  alive <- serverAlive
  if alive
    then do
      void sendStop
      pure (EvAppendLog "Capture server stopped")
    else pure (EvAppendLog "Capture server is not running")
stopCaptureServer (Just chan) = do
  writeChan chan CmdStopCapture
  pure (EvAppendLog "Sent stop command to capture server")

startCaptureServer :: (CCEvent -> IO ()) -> IO ()
startCaptureServer sendMsg = do
  exe <- hortureExePath
  void $ stopCaptureServer Nothing
  print $ "Starting capture server: " <> exe
  sendMsg $ EvAppendLog ("Starting capture server...")
  void $ spawnServerProcess exe ["--server"]
  cmdChan <- newChan
  waitUntilUp 10 >>= \case
    False -> sendMsg (EvAppendLog "Failed to start capture server")
    True -> do
      sendMsg (EvAppendLog "Capture server started")
      sendMsg (EvAppendLog "Click on an application window to capture it.")
      sendMsg (EvConnectionEstablished cmdChan)
      interactWithHorture onReply cmdChan
  where
    onReply :: CCReply -> IO ()
    onReply ROk = pure ()
    onReply (RErr msg) = sendMsg (EvAppendLog ("Error from server: " <> msg))
    onReply (ROut line) = sendMsg (EvAppendLog line)
    onReply (RFPS fps) = sendMsg (EvTickFPS fps)
    onReply (RCapturedWindow title) = do
      sendMsg (EvAppendLog ("Captured window: " <> title))
      sendMsg (EvCapturedWindow title)
    onReply _ = pure ()

showt :: (Show a) => a -> Text
showt = T.pack . show
