{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Horture.CommandCenter.CommandCenter
  ( runCommandCenter,
  )
where

import Brick
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center
import Control.Concurrent (forkOS)
import Control.Concurrent.Chan.Synchronous
import Control.Monad.Except
import Data.Default
import Graphics.Vty hiding (Event)
import Horture.Command
import Horture.CommandCenter.State
import Horture.Event
import Run

data Name = Main
  deriving (Ord, Show, Eq)

drawUI :: CommandCenterState -> [Widget Name]
drawUI _cs =
  [ withBorderStyle unicode $
      borderWithLabel @Name
        (str "Horture CommandCenter")
        (center (str "Left") <+> vBorder <+> center (str "Right"))
  ]

appEvent :: BrickEvent Name e -> EventM Name CommandCenterState ()
appEvent (VtyEvent (EvKey (KChar 'j') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'k') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'h') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'l') [])) = return ()
appEvent (VtyEvent (EvKey (KChar 'g') [])) = grabHorture
appEvent (VtyEvent (EvKey (KChar 'q') [])) = stopHorture
appEvent _ = return ()

stopHorture :: EventM Name CommandCenterState ()
stopHorture = do
  mchan <- gets _ccEventChan
  case mchan of
    Nothing -> return ()
    Just chan -> liftIO $ writeChan chan (EventCommand Exit)

grabHorture :: EventM Name CommandCenterState ()
grabHorture = do
  evChan <- liftIO $ newChan @Event
  -- TODO: Split intialise up here s.t. we only can push into evChan from the
  -- CommandCenter if horturing started.
  _ <- liftIO . forkOS $ initialise evChan
  modify $ \ccs -> ccs { _ccEventChan = Just evChan }

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
