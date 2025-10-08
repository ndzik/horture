module Horture.Backend.MacOS.Initializer (initialize) where

import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Lens
import Control.Monad (forM_, void, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.RingBuffer as RingBuffer
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
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
import Horture.RenderBridge (c_rb_create, c_rb_destroy, c_rb_start, c_rb_stop)
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
        tv <- liftIO $ newTVarIO Nothing
        pure
          CaptureHandle
            { chStop = pure (),
              chTitle = title,
              chWinId = wid
            }

-- ws <- filter (\(_, n) -> T.length n > 0) <$> liftIO listWindowsMac
-- -- liftIO . forM_ ws $ \(wid, name) -> print $ "WindowID" ++ show wid ++ " - " ++ "Name:" ++ show name
-- -- let (wid, title) = head . reverse $ ws
-- let (wid, title) = findID 1196 ws
-- liftIO $ forM_ ws print
-- liftIO . print $ "Grabbing window: " ++ T.unpack title
-- frameBuffer <- liftIO $ newTVarIO Nothing
-- return $ CaptureHandle {chStop = return (), chFrame = frameBuffer, chTitle = title, chWinId = wid}

findID :: Word64 -> [(Word64, Text)] -> (Word64, Text)
findID _def [] = (0, "No Window")
findID def xs = go xs
  where
    go [] = (def, "Default Window")
    go (x@(wid, _) : rest)
      | wid == def = x
      | otherwise = go rest

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

  rb <- liftIO c_rb_create
  rc <- liftIO $ c_rb_start rb (fromIntegral $ chWinId capHandle)
  when (rc /= 0) $
    throwError $
      WindowEnvironmentInitializationErr ("c_rb_start failed: " ++ show rc)

  storage <- liftIO $ newTVarIO Nothing
  evBuf <- liftIO $ RingBuffer.new 4
  fftBuf <- liftIO $ RingBuffer.new 8
  let wa_width = 1028
      wa_height = 720
  (hsp, dip, hbp, ftp) <- liftIO $ initResources (wa_width, wa_height) [] Nothing

  liftIO $ GLFW.setFramebufferSizeCallback glW (Just resizeWindow')
  liftIO $ GLFW.setWindowSize glW (fromIntegral wa_width) (fromIntegral wa_height)
  liftIO $ GL.viewport $= (Position 0 0, Size (fromIntegral wa_width) (fromIntegral wa_height))
  -- liftIO $ GLFW.setWindowPos glW (  ) (fromIntegral . wa_y $ attr)

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
  let ssf = Map.fromList $ map (\(fp, AudioEffect eff _) -> (eff, fp)) loadedSounds
      hc =
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
    Just chan ->
      liftIO $
        runHorture hs hc (playScene @'Channel scene) >>= \case
          Left err -> writeChan chan . T.pack . show $ err
          _otherwise -> return ()
    Nothing -> void . liftIO $ runHorture hs hc (playScene @'NoLog scene)
  liftIO $ GLFW.destroyWindow glW
  liftIO GLFW.terminate
