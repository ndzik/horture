{-# LANGUAGE TypeApplications #-}

module Horture.CommandCenter.CommandCenter
  ( runCommandCenter,
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center
import Control.Monad (void)
import Data.Default
import Graphics.Vty (defAttr)
import Horture.CommandCenter.State

data Name = Main
  deriving (Ord, Show, Eq)

drawUI :: CommandCenterState -> [Widget Name]
drawUI _cs =
  [ withBorderStyle unicode $
      borderWithLabel @Name (str "Horture CommandCenter")
        (center (str "Left") <+> vBorder <+> center (str "Right"))
  ]

appEvent :: BrickEvent Name e -> EventM Name CommandCenterState ()
appEvent _ = return ()

app :: App CommandCenterState e Name
app =
  App
    { appDraw = drawUI,
      appStartEvent = return (),
      appHandleEvent = appEvent,
      appAttrMap = const $ attrMap defAttr [],
      appChooseCursor = neverShowCursor
    }

runCommandCenter :: IO ()
runCommandCenter = void $ defaultMain app def
