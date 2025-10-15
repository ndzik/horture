{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Horture.CommandCenter.GUI (runCommandCenterUI) where

import Control.Concurrent.Chan.Synchronous (Chan, newChan, writeChan)
import Control.Lens
import Control.Monad (void)
import Data.Default
import Data.List (intersperse)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Horture.Effect (ShaderEffect)
import Horture.Object (BehaviourType, Lifetime (Limited))
import Horture.Server
import Horture.Server.Protocol
import Monomer
import Monomer.Core.Themes.BaseTheme (BaseThemeColors (..), baseTheme)

data UIPane = UIPaneMain | UIPaneConfiguration
  deriving (Eq, Show)

data CCModel = CCModel
  { _mCapturedWin :: Maybe Text,
    _mFPS :: Double,
    _mConn :: Maybe (Chan CCCommand),
    _mAssets :: [FilePath],
    _mSelAsset :: Int,
    _mLogLines :: [Text],
    _mMeta :: Text,
    _mMenu :: UIPane
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
      && (isJust (a ^. mConn) == isJust (b ^. mConn))
      && (a ^. mMeta) == (b ^. mMeta)
      && (a ^. mMenu) == (b ^. mMenu)

instance Default CCModel where
  def =
    CCModel
      { _mCapturedWin = Nothing,
        _mConn = Nothing,
        _mFPS = 0,
        _mAssets = [],
        _mSelAsset = 0,
        _mLogLines = [],
        _mMeta = "No metainformation available",
        _mMenu = UIPaneMain
      }

data CCEvent
  = EvTickFPS Double
  | EvAppendLog Text
  | EvConnectionEstablished (Chan CCCommand)
  | EvSetAssets [FilePath]
  | EvConnectApplication
  | EvStartCapture
  | EvCapturedWindow Text
  | EvSendEffect EffectRequest
  | EvStopCapture
  | EvToggleES
  | EvSetUI UIPane
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
        { inputText = textSecondary,
          labelText = textbaseColor,
          btnText = textbaseColor,
          btnBgBasic = lightColor,
          btnBgHover = baseColor,
          btnBgFocus = darkColor,
          btnBgActive = lightColor,
          btnBgDisabled = gray,
          btnFocusBorder = highlightColor
        }

titleSize :: Double
titleSize = 46

headerSize :: Double
headerSize = 20

buildUI :: WidgetEnv CCModel CCEvent -> CCModel -> WidgetNode CCModel CCEvent
buildUI _ m =
  vstack
    [ hstack
        [ vstack
            [ label "Horture" `styleBasic` [textSize titleSize, textCenter],
              separatorLine `styleBasic` [paddingH 4]
            ],
          spacer_ [width 128],
          menuButtons
        ]
        `styleBasic` [padding 8],
      spacer,
      box_ [expandContent] $ menu (m ^. mMenu) `styleBasic` [radius 6],
      filler
    ]
    `styleBasic` [bgColor baseColor, padding 8]
  where
    menu UIPaneMain =
      vstack
        [ hstack
            [ spacer,
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
                  `styleBasic` [bgColor baseColor, radius 6, padding 4, width 128]
            ]
            `styleBasic` [bgColor lightColor, padding 8],
          hstack
            [ filler,
              boxShadow $ capturedWinW m,
              filler
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
                  `styleBasic` [bgColor lightColor, radius 6, padding 4],
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
                  `styleBasic` [bgColor lightColor, radius 6, padding 4]
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
              `styleBasic` [bgColor lightColor, padding 8, radius 6]
        ]
        `styleBasic` [bgColor baseColor]
    menu UIPaneConfiguration =
      vstack
        [ scroll_ [wheelRate 40] $
            vstack
              [ boxShadow . box_ [expandContent] $
                  vstack
                    [ label "Screen Effects" `styleBasic` [textSize headerSize, textCenter],
                      separatorLine `styleBasic` [padding 4],
                      spacer,
                      buttonGrid
                        4
                        ( map
                            behaviourButton
                            [ minBound
                              .. maxBound :: BehaviourType
                            ]
                        )
                    ]
                    `styleBasic` [bgColor lightColor, padding 4, radius 6],
                spacer,
                boxShadow . box_ [expandContent] $
                  vstack
                    [ label "Shader Effects" `styleBasic` [textSize headerSize, textCenter],
                      separatorLine `styleBasic` [padding 4],
                      spacer,
                      buttonGrid
                        4
                        ( map
                            shaderButton
                            [ minBound
                              .. maxBound :: ShaderEffect
                            ]
                        )
                    ]
                    `styleBasic` [bgColor lightColor, padding 4, radius 6]
              ]
        ]

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

    menuButtons =
      hstack
        [ box mainUIbutton,
          spacer,
          box settingsButton
        ]

    captureButton =
      case m ^. mCapturedWin of
        Nothing ->
          button "Connect" EvConnectApplication
            `styleBasic` [textSize 12, width 100, bgColor baseColor, height 32]
            `styleHover` [bgColor darkColor]
        Just _ ->
          button "Disconnect" EvStopCapture
            `styleBasic` [bgColor softRed, textSize 12, width 100, height 32]
            `styleHover` [bgColor darkColor]

    toggleEventSourceButton =
      button "Toggle" EvToggleES
        `styleBasic` [textSize 12, width 100, height 32]
        `nodeEnabled` (isJust $ m ^. mConn)

    settingsButton =
      button "Settings" (EvSetUI UIPaneConfiguration)
        `styleBasic` [textSize 12]

    mainUIbutton =
      button "Overview" (EvSetUI UIPaneMain)
        `styleBasic` [textSize 12]

defaultLifetime :: Lifetime
defaultLifetime = Limited 10

behaviourButton :: BehaviourType -> WidgetNode CCModel CCEvent
behaviourButton bt =
  button (T.pack $ show bt) (EvSendEffect (ERBehaviour bt defaultLifetime))
    `styleBasic` [width 200, bgColor baseColor, paddingT 4]
    `styleHover` [bgColor darkColor]

shaderButton :: ShaderEffect -> WidgetNode CCModel CCEvent
shaderButton se =
  button (T.pack $ show se) (EvSendEffect (ERShader se defaultLifetime))
    `styleBasic` [width 200, bgColor baseColor, paddingT 4]
    `styleHover` [bgColor darkColor]

buttonGrid :: Int -> [WidgetNode CCModel CCEvent] -> WidgetNode CCModel CCEvent
buttonGrid cols btns =
  box_ [alignCenter, alignMiddle] $ vstack (map (hstack . intersperse spacer) (chunk cols btns)) `styleBasic` [padding 4]
  where
    chunk _ [] = []
    chunk n xs =
      let (ys, zs) = splitAt n xs
       in ys : chunk n zs

-- baseColor, lightColor, darkColor, highlightColor, softRed :: Color
-- darkColor = rgbHex "#f2d279"
-- baseColor = rgbHex "#f4d88b"
-- lightColor = rgbHex "#f8e7b9"
-- highlightColor = rgbHex "#d19f15"
-- softRed = rgbHex "#f5655b"

baseColor :: Color
baseColor = rgbHex "#1E1E1E" -- deep neutral base, not pure black

darkColor :: Color
darkColor = rgbHex "#121212" -- almost black; background panels

lightColor :: Color
lightColor = rgbHex "#2A2A2A" -- subtle separation layers / cards

highlightColor :: Color
highlightColor = rgbHex "#3A6EA5" -- soft gold

softRed :: Color
softRed = rgbHex "#D35F5F" -- muted alert tone that fits the palette

textSecondary :: Color
textSecondary = rgbHex "#A0A0A0"

textbaseColor :: Color
textbaseColor = rgbHex "#DADADA"

_textDisabled :: Color
_textDisabled = rgbHex "#666666"

handleEvent ::
  WidgetEnv CCModel CCEvent ->
  WidgetNode CCModel CCEvent ->
  CCModel ->
  CCEvent ->
  [EventResponse CCModel CCEvent CCModel CCEvent]
handleEvent _ _ m = \case
  EvSetAssets as -> [Model (m & mAssets .~ as & mSelAsset .~ 0)]
  EvTickFPS fps -> [Model (m & mFPS .~ fps)]
  EvAppendLog t -> [Model (m & mLogLines %~ (\xs -> take 500 (t : xs)))]
  EvConnectApplication -> [Producer connectCaptureServer]
  EvStartCapture -> [Task $ startCapturing (m ^. mConn)]
  EvStopCapture ->
    [ Task (stopCaptureServer $ m ^. mConn),
      Model (m & mCapturedWin .~ Nothing & mConn .~ Nothing & mFPS .~ 0)
    ]
  EvCapturedWindow stitle -> [Model (m & mCapturedWin .~ Just stitle)]
  EvSendEffect effReq -> [Task $ sendEffectRequest (m ^. mConn) effReq]
  EvToggleES -> [Task $ toggleEventStream (m ^. mConn)]
  EvConnectionEstablished chan -> [Model (m & mConn ?~ chan), Event EvStartCapture]
  EvSetUI pane -> [Model (m & mMenu .~ pane)]
  EvRefreshES -> []
  EvDisableAll -> []
  EvEnableAll -> []
  EvPurgeAll -> []
  EvNoop -> []
  EvExit -> [Task $ stopCaptureServer (m ^. mConn)]

sendEffectRequest :: Maybe (Chan CCCommand) -> EffectRequest -> IO CCEvent
sendEffectRequest Nothing _ = pure (EvAppendLog "Effect request failed: no connection to server")
sendEffectRequest (Just chan) effReq = do
  writeChan chan (CmdTriggerEffect effReq)
  pure EvNoop

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

connectCaptureServer :: (CCEvent -> IO ()) -> IO ()
connectCaptureServer sendMsg = do
  exe <- hortureExePath
  void $ stopCaptureServer Nothing
  print $ "Starting capture server: " <> exe
  sendMsg $ EvAppendLog ("Starting capture server...")
  void $ spawnServerProcess exe ["--server"]
  cmdChan <- newChan
  waitUntilUp 10 >>= \case
    False -> sendMsg (EvAppendLog "Failed to start capture server")
    True -> do
      sendMsg (EvConnectionEstablished cmdChan)
      sendMsg (EvAppendLog "Capture server started")
      sendMsg (EvAppendLog "Click on an application window to capture it.")
      sendMsg (EvAppendLog "Connected to capture server")
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

startCapturing :: Maybe (Chan CCCommand) -> IO CCEvent
startCapturing Nothing = pure (EvAppendLog "Cannot start capture: no connection to server")
startCapturing (Just chan) = do
  writeChan chan CmdStartCapture
  pure (EvAppendLog "Sent start capture command to server")

showt :: (Show a) => a -> Text
showt = T.pack . show
