{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Horture.Backend.X11.Initializer
  ( initialize,
    HortureInitializer (..),
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.MVar
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bits
import Data.Default
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.RingBuffer as RingBuffer
import Data.Text (Text, pack)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Graphics.X11.Xlib.Extras hiding (Event)
import Horture
import Horture.Audio.Player.Protea
import Horture.Backend.X11.LinuxX11 (CaptureHandle)
import Horture.Error
import Horture.Event
import Horture.Horture
import Horture.Initializer
import Horture.Loader.Asset
import Horture.Logging
import Horture.Program
import Horture.Scene hiding (assets)
import Horture.State
import Horture.WindowGrabber
import Numeric (showHex)

instance
  (HortureLogger (HortureInitializer l hdl), hdl ~ CaptureHandle) =>
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
    (,window,True) <$> liftIO (openDisplay "")

initialize ::
  forall l hdl.
  (CaptureHandle ~ hdl, HortureLogger (HortureInitializer l hdl)) =>
  Scene ->
  [(FilePath, Asset)] ->
  [(FilePath, Asset)] ->
  TVar Int ->
  Maybe (Chan Text) ->
  Chan Event ->
  HortureInitializer l hdl ()
initialize startScene loadedImages loadedSounds frameCounter logChan evChan = do
  glW <- liftIO initGLFW
  (dp, w, isMapped) <- grabAnyWindow

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

  liftIO setDefaultErrorHandler

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

  mFont <- asks (^. defaultFont)
  (hsp, dip, hbp, ftp) <- liftIO $ initResources (fromIntegral ww, fromIntegral wh) loadedImages mFont
  storage <- liftIO $ newTVarIO Nothing
  evBuf <- liftIO $ RingBuffer.new 4
  fftBuf <- liftIO $ RingBuffer.new 8
  let scene = startScene {_assetCache = dip ^. assets}
      hs =
        HortureState
          { _envHandle = (dp, w, isMapped),
            _capture = Just pm,
            _audioRecording = Nothing,
            _audioStorage = storage,
            _audioState = def,
            _frameCounter = frameCounter,
            _mvgAvg = fftBuf,
            _dim = (fromIntegral . wa_width $ attr, fromIntegral . wa_height $ attr),
            _eventList = evBuf
          }
  let ssf = Map.fromList $ map (\(fp, AudioEffect eff _) -> (eff, fp)) loadedSounds
      hc =
        HortureStatic
          { _screenProg = hsp,
            _dynamicImageProg = dip,
            _backgroundProg = hbp,
            _fontProg = ftp,
            _audioEnv = def {staticSoundFiles = ssf},
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
    return
      . join
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
