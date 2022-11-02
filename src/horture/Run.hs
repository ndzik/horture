{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Run
  ( run,
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Lens
import Control.Monad.Except
import Data.Default
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text, pack)
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Rendering.OpenGL as GL hiding (Color, flush, rotate)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Graphics.X11.Xlib.Extras hiding (Event)
import Horture
import Horture.Event
import Horture.Horture
import Horture.Loader.Asset
import Horture.Program
import Horture.Scene
import Horture.State
import System.Exit
import Prelude hiding (readFile)

run :: [(FilePath, Asset)] -> Maybe (Chan Text) -> Chan Event -> Window -> IO ()
run gifs logChan evChan w = do
  glW <- initGLFW
  dp <- openDisplay ""
  let ds = defaultScreen dp
  root <- rootWindow dp ds

  Just (_, meW) <- findMe root dp hortureName
  allocaSetWindowAttributes $ \ptr -> do
    set_event_mask ptr noEventMask
    changeWindowAttributes dp meW cWEventMask ptr
    set_border_pixel ptr 0
    changeWindowAttributes dp meW cWBorderPixel ptr

  attr <- getWindowAttributes dp w
  allocaSetWindowAttributes $ \ptr -> do
    _ <- set_event_mask ptr structureNotifyMask
    changeWindowAttributes dp w cWEventMask ptr

  res <- xCompositeQueryExtension dp
  when (isNothing res) exitFailure

  -- CompositeRedirectManual to avoid unnecessarily drawing the captured
  -- window, which is overlayed anyway by our application.
  _ <- xCompositeRedirectWindow dp w CompositeRedirectManual
  pm <- xCompositeNameWindowPixmap dp w

  let ww = wa_width attr
      wh = wa_height attr
  GLFW.setFramebufferSizeCallback glW (Just resizeWindow')
  GLFW.setWindowSize glW (fromIntegral . wa_width $ attr) (fromIntegral . wa_height $ attr)
  GLFW.setWindowPos glW (fromIntegral . wa_x $ attr) (fromIntegral . wa_y $ attr)

  (hsp, hgp, hbp) <- initResources (fromIntegral ww, fromIntegral wh) gifs
  let scene =
        def
          { _screen = def,
            _gifs = Map.empty,
            _gifCache = hgp ^. assets
          }
      hs =
        HortureState
          { _display = dp,
            _xWin = w,
            _capture = pm,
            _dim = (fromIntegral . wa_width $ attr, fromIntegral . wa_height $ attr)
          }
  let hc =
        HortureStatic
          { _screenProg = hsp,
            _gifProg = hgp,
            _backgroundProg = hbp,
            _eventChan = evChan,
            _logChan = logChan,
            _glWin = glW,
            _backgroundColor = Color4 0.1 0.1 0.1 1
          }
  case logChan of
    Just chan ->
      runHorture hs hc (playScene @'Channel scene) >>= \case
        Left err -> writeChan chan . pack . show $ err
        _otherwise -> return ()
    Nothing -> void $ runHorture hs hc (playScene @'NoLog scene)
  GLFW.destroyWindow glW
  GLFW.terminate
  closeDisplay dp

findMe :: Window -> Display -> String -> IO (Maybe ([[Char]], Window))
findMe root dp me = do
  (_root, _parent, childs) <- queryTree dp root
  alloca $ \ptr -> do
    res <-
      mapM
        ( \c -> do
            _ <- xGetTextProperty dp c ptr wM_CLASS
            r <- peek ptr >>= wcTextPropertyToTextList dp
            if not . null $ r
              then return (Just (r, c))
              else return Nothing
        )
        childs
    return . join
      . find
        ( \case
            (Just (ns, _c)) -> me `elem` ns
            _otherwise -> False
        )
      $ res
