module Horture.Backend.MacOS.Initializer
  ( initialize,
  )
where

import Control.Concurrent (newEmptyMVar, putMVar)
import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Lens
import Control.Monad (void, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader
import Data.Default
import qualified Data.Map as Map
import qualified Data.RingBuffer as RingBuffer
import Data.Text (Text)
import qualified Data.Text as T
import Foreign (Ptr, allocaBytes, nullPtr)
import Foreign.C (CInt, peekCString)
import Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Horture
import Horture.Audio
import Horture.Backend.MacOS.Interface
import Horture.Backend.Types
import Horture.Error
import Horture.Event
import Horture.Horture
import Horture.Initializer
import Horture.Loader.Asset
import Horture.Logging
import Horture.Program
import Horture.RenderBridge (RB, c_rb_create_display, c_rb_create_window, c_rb_destroy, c_rb_start_display, c_rb_start_window, c_rb_stop)
import Horture.Scene hiding (assets)
import Horture.State
import Horture.WindowGrabber

instance
  (CaptureHandle ~ hdl, HortureLogger (HortureInitializer l hdl)) =>
  WindowGrabber hdl (HortureInitializer l hdl)
  where
  grabAnyWindow CaptureWindow = do
    liftIO pickWindowMac >>= \case
      Nothing -> throwError (WindowEnvironmentInitializationErr "user aborted")
      Just (wid, title) -> do
        pure
          CaptureHandle
            { chStop = pure (),
              chTitle = title,
              chWinId = (wid, CaptureWindow)
            }
  grabAnyWindow CaptureDisplay = do
    liftIO pickDisplayMac >>= \case
      Nothing -> throwError (WindowEnvironmentInitializationErr "user aborted")
      Just (did, title) -> do
        pure
          CaptureHandle
            { chStop = pure (),
              chTitle = title,
              chWinId = (did, CaptureDisplay)
            }

instance
  (CaptureHandle ~ hdl, HortureLogger (HortureInitializer l hdl)) =>
  WindowManager hdl (HortureInitializer l hdl)
  where
  setupWindowOverlay glfwWindow = do
    nswin <- liftIO $ GLFW.getCocoaWindow glfwWindow
    when (nswin == nullPtr) $
      throwError $
        WindowEnvironmentInitializationErr "GLFW.getCocoaWindow returned nullPtr"

    liftIO $ c_setup_overlay nswin 1

startCapture :: GLFW.Window -> CaptureType -> CaptureHandle -> HortureInitializer l hdl (Ptr RB, CInt, Text)
startCapture glW CaptureDisplay capHandle = do
  nsWindow <- liftIO $ GLFW.getCocoaWindow glW
  when (nsWindow == nullPtr) $
    throwError $
      WindowEnvironmentInitializationErr "GLFW.getCocoaWindow returned nullPtr"
  liftIO $ print @Text "Starting display capture"
  rb <- liftIO c_rb_create_display
  (rc, title) <- liftIO $ allocaBytes 512 $ \buf -> do
    rc <- c_rb_start_display (fromIntegral . fst . chWinId $ capHandle) nsWindow rb buf 512
    title <- T.pack <$> peekCString buf
    pure (rc, title)
  return (rb, rc, title)
startCapture _ CaptureWindow capHandle = do
  liftIO $ print @Text "Starting window capture"
  rb <- liftIO c_rb_create_window
  (rc, title) <- liftIO $ allocaBytes 512 $ \buf -> do
    rc <- c_rb_start_window (fromIntegral . fst . chWinId $ capHandle) rb buf 512
    title <- T.pack <$> peekCString buf
    pure (rc, title)
  return (rb, rc, title)

initialize ::
  forall l hdl.
  (CaptureHandle ~ hdl, HortureLogger (HortureInitializer l hdl)) =>
  CaptureType ->
  Scene ->
  [(FilePath, Asset)] ->
  [(FilePath, Asset)] ->
  TVar Int ->
  Maybe (Chan Text) ->
  Chan Event ->
  HortureInitializer l hdl ()
initialize captureType startScene loadedImages _loadedSounds frameCounter logChan evChan = do
  capHandle <- grabAnyWindow captureType
  glW <- liftIO initGLFW

  setupWindowOverlay glW

  (rb, rc, title) <- startCapture glW captureType capHandle
  when (rc /= 0) $
    throwError $
      WindowEnvironmentInitializationErr ("c_rb_start failed: " ++ show rc)

  asks (^. grabbedWin) >>= liftIO . flip putMVar title

  storage <- liftIO $ newTVarIO Nothing
  evBuf <- liftIO $ RingBuffer.new 4
  fftBuf <- liftIO $ RingBuffer.new 8
  mFont <- asks (^. defaultFont)
  let wa_width = 1028
      wa_height = 720
  (hsp, dip, hbp, ftp) <- liftIO $ initResources (wa_width, wa_height) loadedImages mFont

  liftIO $ GLFW.setFramebufferSizeCallback glW (Just resizeWindow')
  liftIO $ GLFW.setWindowSize glW (fromIntegral wa_width) (fromIntegral wa_height)
  liftIO $ GL.viewport $= (Position 0 0, Size (fromIntegral wa_width) (fromIntegral wa_height))

  sizeRef <- liftIO $ newTVarIO (wa_width, wa_height)
  tvarEnvHandle <-
    liftIO . newTVarIO $
      CaptureHandle
        { chWinId = chWinId capHandle,
          chTitle = chTitle capHandle,
          chStop = c_rb_stop rb >> c_rb_destroy rb
        }
  tvarRenderBridge <- liftIO $ newTVarIO rb
  mvarAudioRecording <- liftIO newEmptyMVar
  tvarAllBands <- liftIO $ newTVarIO def
  tvarAudioState <- liftIO $ newTVarIO AudioPlayerState {apHandle = nullPtr, apSounds = Map.empty}
  tvarMvAvg <- liftIO $ newTVarIO fftBuf
  tvarEventBuf <- liftIO $ newTVarIO evBuf

  let scene = startScene {_assetCache = dip ^. assets}
  let hc =
        HortureEnv
          { _envHandle = tvarEnvHandle,
            _renderBridgeCtx = tvarRenderBridge,
            _audioRecording = mvarAudioRecording,
            _audioBandState = tvarAllBands,
            _audioStorage = storage,
            _audioState = tvarAudioState,
            _frameCounter = frameCounter,
            _mvgAvg = tvarMvAvg,
            _dim = sizeRef,
            _eventList = tvarEventBuf,
            -- READONLY
            _screenProg = hsp,
            _dynamicImageProg = dip,
            _backgroundProg = hbp,
            _fontProg = ftp,
            _audioEnv = AudioPlayerEnv,
            _eventChan = evChan,
            _logChan = logChan,
            _glWin = glW,
            _backgroundColor = Color4 0.1 0.1 0.1 1
          }
  case logChan of
    Just chan -> do
      liftIO . print @Text $ "Starting with logging to channel"
      liftIO $
        runHorture hc (playScene @'Prod @'Channel scene) >>= \case
          Left err -> writeChan chan . T.pack . show $ err
          _otherwise -> return ()
    Nothing -> do
      liftIO . print @Text $ "Starting without logging"
      void . liftIO $ runHorture hc (playScene @'Prod @'NoLog scene)
  liftIO $ GLFW.destroyWindow glW
  liftIO GLFW.terminate
