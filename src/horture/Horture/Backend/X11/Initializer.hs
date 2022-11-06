{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Horture.Backend.X11.Initializer
  ( initialize,
    HortureInitializer (..),
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.MVar
import Control.Concurrent.STM (newTVarIO)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bits
import Data.Default
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Graphics.X11.Xlib.Extras hiding (Event)
import Horture
import Horture.Backend.X11.LinuxX11 ()
import Horture.Error
import Horture.Event
import Horture.Horture
import Horture.Initializer
import Horture.Loader.Asset
import Horture.Logging
import Horture.Program
import Horture.Scene
import Horture.State
import Horture.WindowGrabber
import Numeric (showHex)

instance
  (HortureLogger (HortureInitializer l hdl), hdl ~ (Display, Window)) =>
  WindowGrabber hdl (HortureInitializer l hdl)
  where
  grabAnyWindow = do
    mv <- asks (^. grabbedWin)
    (name, window) <-
      liftIO x11UserGrabWindow >>= \case
        Nothing -> do
          liftIO $ putMVar mv Nothing
          throwError . WindowEnvironmentInitializationErr $ "user aborted window selection"
        Just res -> return res
    liftIO . putMVar mv . Just . unlines $ ["Capturing: " <> name, "WinID: 0x" <> showHex window ""]
    logInfo . pack . unwords $ ["User selected application to grab:", name]
    (,window) <$> liftIO (openDisplay "")

initialize ::
  forall l hdl.
  ((Display, Window) ~ hdl, HortureLogger (HortureInitializer l hdl)) =>
  [(FilePath, Asset)] ->
  Maybe (Chan Text) ->
  Chan Event ->
  HortureInitializer l hdl ()
initialize gifs logChan evChan = do
  glW <- liftIO initGLFW
  (dp, w) <- grabAnyWindow

  let ds = defaultScreen dp
  root <- liftIO $ rootWindow dp ds

  meW <-
    liftIO (findMe root dp hortureName) >>= \case
      Nothing -> throwError WindowEnvironmentQueryHortureErr
      Just (_, meW) -> return meW

  liftIO $
    allocaSetWindowAttributes $ \ptr -> do
      set_event_mask ptr noEventMask
      changeWindowAttributes dp meW cWEventMask ptr
      set_border_pixel ptr 0
      changeWindowAttributes dp meW cWBorderPixel ptr

  liftIO $
    allocaSetWindowAttributes $ \ptr -> do
      _ <- set_event_mask ptr structureNotifyMask
      changeWindowAttributes dp w cWEventMask ptr

  liftIO (xCompositeQueryExtension dp) >>= \case
    Nothing ->
      throwError . WindowEnvironmentInitializationErr $
        "X11 unable to query Xlibcomposite extension"
    _otherwise -> return ()

  -- CompositeRedirectManual to avoid unnecessarily drawing the captured
  -- window, which is overlayed anyway by our application.
  liftIO $ xCompositeRedirectWindow dp w CompositeRedirectManual
  pm <- liftIO $ xCompositeNameWindowPixmap dp w

  attr <- liftIO $ getWindowAttributes dp w
  let ww = wa_width attr
      wh = wa_height attr
  liftIO $ GLFW.setFramebufferSizeCallback glW (Just resizeWindow')
  liftIO $ GLFW.setWindowSize glW (fromIntegral . wa_width $ attr) (fromIntegral . wa_height $ attr)
  liftIO $ GLFW.setWindowPos glW (fromIntegral . wa_x $ attr) (fromIntegral . wa_y $ attr)

  (hsp, hgp, hbp) <- liftIO $ initResources (fromIntegral ww, fromIntegral wh) gifs
  storage <- liftIO $ newTVarIO Nothing
  let scene =
        def
          { _screen = def,
            _gifs = Map.empty,
            _gifCache = hgp ^. assets
          }
      hs =
        HortureState
          { _envHandle = (dp, w),
            _capture = pm,
            _audioRecording = Nothing,
            _audioStorage = storage,
            _mvgAvg = [],
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
      liftIO $
        runHorture hs hc (playScene @'Channel scene) >>= \case
          Left err -> writeChan chan . pack . show $ err
          _otherwise -> return ()
    Nothing -> void . liftIO $ runHorture hs hc (playScene @'NoLog scene)
  liftIO $ GLFW.destroyWindow glW
  liftIO GLFW.terminate
  liftIO $ closeDisplay dp

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

x11UserGrabWindow :: IO (Maybe (String, Window))
x11UserGrabWindow = do
  dp <- openDisplay ""
  let ds = defaultScreen dp
  root <- rootWindow dp ds
  cursor <- createFontCursor dp xC_crosshair
  _ <-
    grabPointer
      dp
      root
      False
      ( buttonMotionMask
          .|. buttonPressMask
          .|. buttonReleaseMask
      )
      grabModeAsync
      grabModeAsync
      root
      cursor
      currentTime

  userDecision <- allocaXEvent $ \evptr -> do
    nextEvent dp evptr
    getEvent evptr >>= \case
      ButtonEvent {..} -> do
        alloca $ \cptr -> do
          s <- xFetchName dp ev_subwindow cptr
          if s == 0
            then return . Just $ ("unknown", ev_subwindow)
            else peek cptr >>= peekCString >>= \n -> return . Just $ (n, ev_subwindow)
      _otherwise -> return Nothing

  ungrabPointer dp currentTime
  freeCursor dp cursor
  closeDisplay dp
  return userDecision
