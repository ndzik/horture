{-# LANGUAGE ForeignFunctionInterface #-}

module Horture.Backend.MacOS.Interface
  ( listWindowsMac,
    checkScreenPermission,
    requestScreenPermission,
    resetScreenPermission,
    pickWindowMac,
    CaptureHandle (..),
  )
where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Lens
import Control.Monad (when)
import qualified Control.Monad as Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import Foreign
import Foreign.C
import Graphics.GL (glPixelStorei)
import Graphics.GL.Tokens
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Horture.Asset (HasTextureObject (textureObject))
import Horture.Backend.Types
import Horture.Horture
import Horture.Logging
import Horture.Program (HasTextureUnit (textureUnit))
import Horture.RenderBridge
import Horture.RenderBridge (rbPeekWidth)
import Horture.State
import Horture.WindowGrabber

foreign import ccall "SCShim.h sc_list_windows"
  c_sc_list_windows :: FunPtr (CUIntMax -> CString -> Ptr () -> IO ()) -> Ptr () -> IO ()

foreign import ccall "SCShim.h sc_start_window"
  c_sc_start_window ::
    CUIntMax ->
    FunPtr (Ptr Word8 -> CInt -> CInt -> CInt -> Ptr () -> IO ()) ->
    FunPtr (Ptr () -> IO ()) ->
    Ptr () ->
    IO CInt

foreign import ccall safe "SCShim.h sc_pick_window"
  c_sc_pick_window :: FunPtr (CUIntMax -> CString -> Ptr () -> IO ()) -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  mkPickCB :: (CUIntMax -> CString -> Ptr () -> IO ()) -> IO (FunPtr (CUIntMax -> CString -> Ptr () -> IO ()))

foreign import ccall "SCShim.h sc_stop"
  c_sc_stop :: IO ()

foreign import ccall "wrapper"
  mkWinCB :: (CUIntMax -> CString -> Ptr () -> IO ()) -> IO (FunPtr (CUIntMax -> CString -> Ptr () -> IO ()))

foreign import ccall "wrapper"
  mkFrameCB :: (Ptr Word8 -> CInt -> CInt -> CInt -> Ptr () -> IO ()) -> IO (FunPtr (Ptr Word8 -> CInt -> CInt -> CInt -> Ptr () -> IO ()))

foreign import ccall "wrapper"
  mkStopCB :: (Ptr () -> IO ()) -> IO (FunPtr (Ptr () -> IO ()))

foreign import ccall "sc_preflight_screen"
  c_sc_preflight_screen :: IO CInt

foreign import ccall "sc_request_screen"
  c_sc_request_screen :: IO CInt

foreign import ccall "sc_tcc_reset"
  c_sc_tcc_reset :: CString -> IO CInt

foreign import ccall "sc_get_window_rect"
  c_sc_get_window_rect :: CUIntMax -> Ptr SCRect -> IO CInt

data SCRect = SCRect {_rx :: CInt, _ry :: CInt, _rw :: CInt, _rh :: CInt}

instance Storable SCRect where
  sizeOf _ = 16
  alignment _ = alignment (undefined :: CInt)
  peek p = SCRect <$> peekByteOff p 0 <*> peekByteOff p 4 <*> peekByteOff p 8 <*> peekByteOff p 12
  poke p (SCRect x y w h) = do
    pokeByteOff p 0 x >> pokeByteOff p 4 y >> pokeByteOff p 8 w >> pokeByteOff p 12 h

listWindowsMac :: IO [(Word64, T.Text)]
listWindowsMac = do
  acc <- newIORef []
  let f wid cstr _ = do
        t <- TF.peekCString cstr
        modifyIORef' acc $ \cur -> ((fromIntegral wid, t) : cur)
  cb <- mkWinCB f
  c_sc_list_windows cb nullPtr
  readIORef acc

checkScreenPermission :: IO Bool
checkScreenPermission = (/= 0) <$> c_sc_preflight_screen

requestScreenPermission :: IO Bool
requestScreenPermission = (/= 0) <$> c_sc_request_screen

-- Pass Nothing to reset all ScreenCapture entries, or Just "io.alacritty" etc.
resetScreenPermission :: Maybe String -> IO Int
resetScreenPermission mBid =
  case mBid of
    Nothing -> c_sc_tcc_reset nullPtr >>= pure . fromIntegral
    Just bid -> withCString bid (\c -> c_sc_tcc_reset c) >>= pure . fromIntegral

-- Frame copy that survives callback
data FrameCopy = FrameCopy
  { fcWidth :: !Int,
    fcHeight :: !Int,
    fcStride :: !Int,
    fcFmt :: !PixelFmt,
    fcBytes :: !(ForeignPtr Word8) -- owns the pixels
  }

data CaptureHandle = CaptureHandle
  { chWinId :: !Word64,
    chTitle :: !T.Text,
    chStop :: !(IO ()) -- stop action
  }

instance Show CaptureHandle where
  show h = "CaptureHandle {chWinId = " ++ show (chWinId h) ++ ", chTitle = " ++ T.unpack (chTitle h) ++ "}"

instance
  (CaptureHandle ~ hdl, HortureLogger (Horture l hdl)) =>
  WindowPoller hdl (Horture l hdl)
  where
  pollWindowEnvironment = do
    h <- gets (^. envHandle)
    win <- asks (^. glWin)
    let wid = chWinId h
    rc <- liftIO $ alloca $ \p -> do
      r <- c_sc_get_window_rect (fromIntegral wid) p
      if r == 0 then Just <$> peek p else pure Nothing
    case rc of
      Nothing -> pure ()
      Just (SCRect x y w h) -> liftIO $ do
        let gap = 1
            overlaxX = fromIntegral $ x + gap
            overlaxY = fromIntegral $ y + gap
            overlayW = fromIntegral $ w - 2 * gap
            overlayH = fromIntegral $ h - 2 * gap
        GLFW.setWindowPos win overlaxX overlaxY
        GLFW.setWindowSize win overlayW overlayH
  nextFrame = do
    rb <- gets (^. renderBridgeCtx)
    texUnit <- asks (^. screenProg . textureUnit)
    texObj <- asks (^. screenProg . textureObject)
    sizeRef <- gets (^. dim)
    -- Ensure the target texture is bound!
    liftIO $ do
      activeTexture $= texUnit
      textureBinding Texture2D $= Just texObj
      alloca $ \pf -> do
        rc <- c_rb_poll rb pf
        when (rc == 1) $ do
          w <- rbPeekWidth pf
          h <- rbPeekHeight pf
          (aw, ah) <- readTVarIO sizeRef
          when (w /= aw || h /= ah) $ do
            -- allocate storage once or on resize
            glPixelStorei GL_UNPACK_ALIGNMENT 1
            texImage2D
              Texture2D
              NoProxy
              0
              GL.RGBA8
              (TextureSize2D w h)
              0
              (PixelData BGRA UnsignedByte nullPtr)
            atomically $ writeTVar sizeRef (w, h)

          _ <- c_rb_upload rb pf -- bridge sets UNPACK_ROW_LENGTH and calls glTexSubImage2D
          c_rb_release rb pf

pickWindowMac :: IO (Maybe (Word64, T.Text))
pickWindowMac = do
  mv <- newEmptyMVar
  cb <- mkPickCB $ \wid cstr _ -> do
    t <- T.pack <$> peekCString cstr
    putMVar mv (Just (fromIntegral wid, t))
  print @String $ "Please pick a window (ESC to abort)"
  r <- c_sc_pick_window cb nullPtr
  if r == 0
    then do
      print @String $ "Window picked"
      r <- takeMVar mv
      print @String $ "Window picking finished"
      pure r
    else do
      print @String $ "Window picking aborted"
      pure Nothing
