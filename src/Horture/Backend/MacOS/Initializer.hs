module Horture.Backend.MacOS.Initializer (initialize) where

import Control.Concurrent (putMVar)
import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Lens
import Control.Monad (void, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.RingBuffer as RingBuffer
import Data.Text (Text)
import qualified Data.Text as T
import Foreign (Ptr, allocaBytes)
import Foreign.C (CInt, peekCString)
import Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Horture
import Horture.Backend.MacOS.Interface
import Horture.Error
import Horture.Event
import Horture.Horture
import Horture.Initializer
import Horture.Loader.Asset
import Horture.Logging
import Horture.Program
import Horture.RenderBridge (RB, c_rb_create, c_rb_destroy, c_rb_start, c_rb_stop)
import Horture.Scene hiding (assets)
import Horture.State
import Horture.WindowGrabber

instance
  (CaptureHandle ~ hdl, HortureLogger (HortureInitializer l hdl)) =>
  WindowGrabber hdl (HortureInitializer l hdl)
  where
  grabAnyWindow = do
    liftIO pickWindowMac >>= \case
      Nothing -> throwError (WindowEnvironmentInitializationErr "user aborted")
      Just (wid, title) -> do
        pure
          CaptureHandle
            { chStop = pure (),
              chTitle = title,
              chWinId = wid
            }

startCapture :: CaptureHandle -> HortureInitializer l hdl (Ptr RB, CInt, Text)
startCapture capHandle = do
  rb <- liftIO c_rb_create
  (rc, title) <- liftIO $ allocaBytes 512 $ \buf -> do
    rc <- c_rb_start (fromIntegral $ chWinId capHandle) rb buf 512
    title <- T.pack <$> peekCString buf
    pure (rc, title)
  return (rb, rc, title)

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
  capHandle <- grabAnyWindow

  (rb, rc, title) <- startCapture capHandle
  when (rc /= 0) $
    throwError $
      WindowEnvironmentInitializationErr ("c_rb_start failed: " ++ show rc)

  asks (^. grabbedWin) >>= liftIO . flip putMVar title

  storage <- liftIO $ newTVarIO Nothing
  evBuf <- liftIO $ RingBuffer.new 4
  fftBuf <- liftIO $ RingBuffer.new 8
  let wa_width = 1028
      wa_height = 720
  (hsp, dip, hbp, ftp) <- liftIO $ initResources (wa_width, wa_height) loadedImages Nothing

  liftIO $ GLFW.setFramebufferSizeCallback glW (Just resizeWindow')
  liftIO $ GLFW.setWindowSize glW (fromIntegral wa_width) (fromIntegral wa_height)
  liftIO $ GL.viewport $= (Position 0 0, Size (fromIntegral wa_width) (fromIntegral wa_height))

  sizeRef <- liftIO $ newTVarIO (wa_width, wa_height)

  let scene = startScene {_assetCache = dip ^. assets}
      hs =
        HortureState
          { _envHandle =
              CaptureHandle
                { chWinId = chWinId capHandle,
                  chTitle = chTitle capHandle,
                  chStop = c_rb_stop rb >> c_rb_destroy rb
                },
            _renderBridgeCtx = rb,
            _capture = Nothing,
            _audioRecording = Nothing,
            _audioStorage = storage,
            _audioState = AudioPlayerState,
            _frameCounter = frameCounter,
            _mvgAvg = fftBuf,
            _dim = sizeRef,
            _eventList = evBuf
          }
  let hc =
        HortureStatic
          { _screenProg = hsp,
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
        runHorture hs hc (playScene @'Channel scene) >>= \case
          Left err -> writeChan chan . T.pack . show $ err
          _otherwise -> return ()
    Nothing -> do
      liftIO . print @Text $ "Starting without logging"
      void . liftIO $ runHorture hs hc (playScene @'NoLog scene)
  liftIO $ GLFW.destroyWindow glW
  liftIO GLFW.terminate
