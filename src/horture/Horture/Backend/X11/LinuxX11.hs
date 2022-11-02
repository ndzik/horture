{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Horture.Backend.X11.LinuxX11 where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Foreign.Ptr
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Horture.Backend.X11.X11
import Horture.GL
import Horture.Gif
import Horture.Horture
import Horture.Logging
import Horture.Program
import Horture.State
import Horture.WindowGrabber

instance
  ((Display, Window) ~ hdl, HortureLogger (Horture l hdl)) =>
  WindowPoller hdl (Horture l hdl)
  where
  pollWindowEnvironment = pollXEvents
  nextFrame = captureApplicationFrame

-- | Updates currently bound texture with the pixeldata of the frame for the
-- captured application.
captureApplicationFrame ::
  (HortureLogger (Horture l (Display, Window))) =>
  Horture l (Display, Window) ()
captureApplicationFrame = do
  (dp, pm) <- gets (^. envHandle)
  dim <- gets (^. dim)
  liftIO $ getWindowImage dp pm dim >>= updateWindowTexture dim

-- | getWindowImage fetches the image of the currently captured application
-- window.
getWindowImage :: Display -> Drawable -> (Int, Int) -> IO Image
getWindowImage dp pm (w, h) =
  getImage
    dp
    pm
    1
    1
    (fromIntegral w)
    (fromIntegral h)
    0xFFFFFFFF
    zPixmap

-- | updateWindowTexture updates the OpenGL texture for the captured window
-- using the given dimensions together with the source image as a data source.
updateWindowTexture :: (Int, Int) -> Image -> IO ()
updateWindowTexture (w, h) i = do
  src <- ximageData i
  let pd = PixelData BGRA UnsignedInt8888Rev src
  texSubImage2D
    Texture2D
    0
    (TexturePosition2D 0 0)
    (TextureSize2D (fromIntegral w) (fromIntegral h))
    pd
  destroyImage i

pollXEvents :: Horture l (Display, Window) ()
pollXEvents = do
  glWin <- asks _glWin
  modelUniform <- asks (^. screenProg . modelUniform)
  projectionUniform <- asks (^. screenProg . projectionUniform)
  backTexObj <- asks (^. screenProg . backTextureObject)
  screenTexObj <- asks (^. screenProg . textureObject)
  screenTexUnit <- asks (^. screenProg . textureUnit)
  (dp, xWin) <- gets (^. envHandle)
  pm <- gets _capture
  (oldW, oldH) <- gets _dim
  (pm, (newW, newH)) <- liftIO $
    allocaXEvent $ \evptr -> do
      doIt <- checkWindowEvent dp xWin structureNotifyMask evptr
      if doIt
        then do
          getEvent evptr >>= \case
            ConfigureEvent {..} -> do
              -- Retrieve a new pixmap
              newPm <- xCompositeNameWindowPixmap dp xWin
              -- Update reference, aspect ratio & destroy old pixmap.
              freePixmap dp pm
              -- Update overlay window with new aspect ratio.
              let newWInt = fromIntegral ev_width
                  newHInt = fromIntegral ev_height
                  newWFloat = fromIntegral ev_width
                  newHFloat = fromIntegral ev_height
              GLFW.setWindowSize glWin newWInt newHInt
              GLFW.setWindowPos glWin (fromIntegral ev_x) (fromIntegral ev_y)
              let !anyPixelData = PixelData BGRA UnsignedInt8888Rev nullPtr
              -- Update texture bindings!
              activeTexture $= screenTexUnit
              textureBinding Texture2D $= Just screenTexObj
              texImage2D
                Texture2D
                NoProxy
                0
                RGBA'
                (TextureSize2D (fromIntegral ev_width) (fromIntegral ev_height))
                0
                anyPixelData
              generateMipmap' Texture2D
              textureBinding Texture2D $= Just backTexObj
              texImage2D
                Texture2D
                NoProxy
                0
                RGBA'
                (TextureSize2D (fromIntegral ev_width) (fromIntegral ev_height))
                0
                anyPixelData
              generateMipmap' Texture2D

              -- TODO: WHY does this have no effect?
              let proj = projectionForAspectRatio (newWFloat, newHFloat)
              m44ToGLmatrix proj >>= (uniform projectionUniform $=)

              let model = scaleForAspectRatio (newWInt, newHInt)
              m44ToGLmatrix model >>= (uniform modelUniform $=)

              return (newPm, (newWInt, newHInt))
            _otherwise -> return (pm, (oldW, oldH))
        else return (pm, (oldW, oldH))
  modify $ \hs -> hs {_dim = (newW, newH), _capture = pm}
