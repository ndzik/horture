{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Horture.CommandCenter.GUI (runCommandCenterUI) where

import Control.Lens
import Data.Default
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Horture.CommandCenter.InsetBoxShadow
import Monomer
import Monomer.Core.Themes.BaseTheme (BaseThemeColors (..), baseTheme)

data CCModel = CCModel
  { _mCapturedWin :: Maybe Text,
    _mFPS :: Double,
    _mAssets :: [FilePath],
    _mSelAsset :: Int,
    _mLogLines :: [Text],
    _mMeta :: Text,
    _mFocusPane :: Pane
  }
  deriving (Eq, Show, Generic)

data Pane = PaneAssets | PaneLog
  deriving (Eq, Show, Generic)

makeLenses 'CCModel

instance Default CCModel where
  def =
    CCModel
      { _mCapturedWin = Nothing,
        _mFPS = 0,
        _mAssets = [],
        _mSelAsset = 0,
        _mLogLines = [],
        _mMeta = "No metainformation available",
        _mFocusPane = PaneLog
      }

data CCEvent
  = EvTickFPS Double
  | EvAppendLog Text
  | EvSetAssets [FilePath]
  | EvStartCapture
  | EvStopCapture
  | EvToggleES
  | EvRefreshES
  | EvDisableAll
  | EvEnableAll
  | EvPurgeAll
  | EvExit
  deriving (Eq, Show)

runCommandCenterUI :: [FilePath] -> IO ()
runCommandCenterUI initialAssets = do
  let cfg =
        [ appWindowTitle "Horture",
          appTheme customTheme,
          appInitEvent (EvSetAssets initialAssets),
          appFontDef "Regular" "./assets/fonts/CaskaydiaMonoNerdFontMono-Regular.ttf",
          appWindowState (MainWindowNormal (1200, 800))
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
    [ hstack
        [ vstack
            [ label "CommandCenter" `styleBasic` [textSize titleSize, textCenter],
              separatorLine `styleBasic` [paddingH 4]
            ],
          filler,
          box $
            button "Start Capture" EvStartCapture,
          filler,
          boxShadow $
            (capturedWinW m)
              `styleBasic` [bgColor mustardLight, radius 6, padding 4],
          spacer_ [width 32],
          boxShadow . box $
            ( vstack
                [ label "FPS" `styleBasic` [textSize headerSize, textCenter],
                  separatorLine,
                  spacer,
                  (label (showt (m ^. mFPS))) `styleBasic` [textCenter]
                ]
            )
              `styleBasic` [bgColor mustardLight, radius 6, padding 4]
        ]
        `styleBasic` [padding 8],
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

mustardBase, mustardLight, mustardDark, mustardHighlight, grayBG, grayPanel, offWhite :: Color
mustardDark = rgbHex "#f2d279"
mustardBase = rgbHex "#f4d88b"
mustardLight = rgbHex "#f8e7b9"
mustardHighlight = rgbHex "#d19f15"
grayBG = rgbHex "#1E1E1E"
grayPanel = rgbHex "#2A2A2A"
offWhite = rgbHex "#F2F2F2"

handleEvent ::
  WidgetEnv CCModel CCEvent ->
  WidgetNode CCModel CCEvent ->
  CCModel ->
  CCEvent ->
  [EventResponse CCModel CCEvent CCModel CCEvent]
handleEvent _ _ m = \case
  -- init/data updates
  EvSetAssets as ->
    [Model (m & mAssets .~ as & mSelAsset .~ 0)]
  EvTickFPS fps ->
    [Model (m & mFPS .~ fps)]
  EvAppendLog t ->
    [ Model (m & mLogLines %~ (\xs -> takeLast 500 (xs ++ [t]))),
      Task $ pure (EvTickFPS (m ^. mFPS)) -- no-op tick; keep UI live
    ]
  -- commands (hook your IPC here)
  EvStartCapture -> [Model m] -- TODO: trigger window picker/IPC
  EvStopCapture -> [Model (m & mCapturedWin .~ Nothing)]
  EvToggleES -> [Model m]
  EvRefreshES -> [Model m]
  EvDisableAll -> [Model m]
  EvEnableAll -> [Model m]
  EvPurgeAll -> [Model m]
  EvExit -> [exitApplication]

-- ===== Key bindings (global)
-- Add this in buildUI if you want global hotkeys:
-- keystroke
--   [ ("G", EvStartCapture), ("g", EvStartCapture)
--   , ("Q", EvStopCapture), ("q", EvStopCapture)
--   , ("S", EvToggleES), ("s", EvToggleES)
--   , ("R", EvRefreshES), ("r", EvRefreshES)
--   , ("D", EvDisableAll), ("d", EvDisableAll)
--   , ("E", EvEnableAll), ("e", EvEnableAll)
--   , ("P", EvPurgeAll), ("p", EvPurgeAll)
--   , ("Esc", EvExit)
--   ] (buildUI …)

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n `max` 0) xs

showt :: (Show a) => a -> Text
showt = T.pack . show
