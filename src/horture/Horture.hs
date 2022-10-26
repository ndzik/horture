{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Horture
  ( SizeUpdate (..),
    hortureName,
    resizeWindow',
    verts,
    floatSize,
    planeVertsSize,
    vertsElement,
    vertsElementSize,
    ximageData,
    initResources,
    initGLFW,
    m44ToGLmatrix,
    playScene,
    identityM44,
    x11UserGrabWindow,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLUtil as Util hiding (throwError)
import Graphics.Rendering.OpenGL as GL hiding (Color, flush)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11 hiding (resizeWindow)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import Horture.Effect
import Horture.Error
import Horture.Events
import Horture.Gif
import Horture.Horture
import Horture.Logging
import Horture.Program
import Horture.Render
import Horture.Scene
import Horture.Shader.Shader
import Horture.State
import System.Exit

hortureName :: String
hortureName = "horture"

-- | playScene plays the given scene in a Horture context.
playScene :: (HortureLogger (Horture l)) => Scene -> Horture l ()
playScene s = do
  setTime 0
  go 0 (Just s)
  where
    go _ Nothing = return ()
    go startTime (Just s) = do
      dt <- deltaTime startTime
      clearView
      renderScene dt s
      renderGifs dt . _gifs $ s
      updateView
      s' <- getTime >>= \timeNow -> pollEvents s timeNow dt <&> (purge timeNow <$>)
      go startTime s' `catchError` handleHortureError >> go startTime (Just s)
    handleHortureError (HE err) = logError . pack $ err

clearView :: Horture l ()
clearView = liftIO $ GL.clear [ColorBuffer, DepthBuffer]

updateView :: Horture l ()
updateView = asks _glWin >>= liftIO . GLFW.swapBuffers

pollEvents :: (HortureLogger (Horture l)) => Scene -> Double -> Double -> Horture l (Maybe Scene)
pollEvents s timeNow dt = do
  pollGLFWEvents
  pollXEvents
  pollHortureEvents timeNow dt s

pollGLFWEvents :: Horture l ()
pollGLFWEvents = liftIO GLFW.pollEvents

pollXEvents :: Horture l ()
pollXEvents = do
  glWin <- asks _glWin
  modelUniform <- asks (^. screenProg . modelUniform)
  projectionUniform <- asks (^. screenProg . projectionUniform)
  backTexObj <- asks (^. screenProg . backTextureObject)
  screenTexObj <- asks (^. screenProg . textureObject)
  screenTexUnit <- asks (^. screenProg . textureUnit)
  xWin <- gets _xWin
  dp <- gets _display
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

deltaTime :: Double -> Horture l Double
deltaTime startTime =
  getTime >>= \currentTime -> return $ currentTime - startTime

setTime :: Double -> Horture l ()
setTime = liftIO . GLFW.setTime

getTime :: Horture l Double
getTime =
  liftIO GLFW.getTime >>= \case
    Nothing -> throwError . HE $ "GLFW not running or initialized"
    Just t -> return t

verts :: [Float]
verts = [-1, -1, 0, 0, 1, -1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, -1, 0, 1, 1]

vertsElement :: [GLuint]
vertsElement = [0, 1, 2, 0, 2, 3]

uintSize :: Int
uintSize = sizeOf @GLuint undefined

floatSize :: Int
floatSize = sizeOf @Float undefined

planeVertsSize :: Int
planeVertsSize = length verts * floatSize

vertsElementSize :: Int
vertsElementSize = length vertsElement * uintSize

ximageData :: Image -> IO (Ptr Word8)
ximageData (Image p) = peek (plusPtr @Image @(Ptr CIntPtr) p xdataPtr) >>= \buf -> return . castPtr $ buf
  where
    szCint = sizeOf @CInt undefined
    xdataPtr = 4 * szCint

initGLFW :: IO GLFW.Window
initGLFW = do
  i <- GLFW.init
  unless i $ print @String "GLFW.init failed" >> exitFailure

  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'Floating True
  GLFW.windowHint $ GLFW.WindowHint'FocusOnShow False
  GLFW.windowHint $ GLFW.WindowHint'Focused False
  GLFW.windowHint $ GLFW.WindowHint'Decorated False
  GLFW.windowHint $ GLFW.WindowHint'MousePassthrough True
  win <-
    GLFW.createWindow 1024 1024 hortureName Nothing Nothing >>= \case
      Nothing -> throwError . userError $ "Failed to create GLFW window"
      Just win -> return win
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowCloseCallback win (Just shutdown)

  return win

resizeWindow' :: GLFW.WindowSizeCallback
resizeWindow' _ w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

shutdown :: GLFW.Window -> IO ()
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  void exitSuccess

-- TODO: Move HortureScreenProgram & HortureGifProgram initialisation code into
-- this function and return the compound structs instead of this ugly big
-- tuple.
initResources :: IO (VertexArrayObject, BufferObject, BufferObject, Program, Program, Map.Map ShaderEffect [Program])
initResources = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  vsp <- loadShaderBS "mvp.shader" VertexShader mvpVertexShader
  fsp <- loadShaderBS "display.shader" FragmentShader displayShader
  prog <- linkShaderProgram [vsp, fsp]
  effs <- loadShaderBS "passthrough.shader" VertexShader passthroughVertexShader >>= compileAndLinkShaderEffects
  vspg <- loadShaderBS "gifvertex.shader" VertexShader gifVertexShader
  fspg <- loadShaderBS "giffragment.shader" FragmentShader gifFragmentShader
  gifProg <- linkShaderProgram [vspg, fspg]
  currentProgram $= Just prog
  veo <- genObjectName
  bindBuffer ArrayBuffer $= Just veo
  return (vao, vbo, veo, prog, gifProg, effs)
  where
    compileAndLinkShaderEffects vsp = do
      barrelProg <- loadShaderBS "barrel.shader" FragmentShader barrelShader >>= linkShaderProgram . (: [vsp])
      stitchProg <- loadShaderBS "stitch.shader" FragmentShader stitchShader >>= linkShaderProgram . (: [vsp])
      blurVProg <- loadShaderBS "blurv.shader" FragmentShader blurVShader >>= linkShaderProgram . (: [vsp])
      blurHProg <- loadShaderBS "blurh.shader" FragmentShader blurHShader >>= linkShaderProgram . (: [vsp])
      flashbangProg <- loadShaderBS "flashbang.shader" FragmentShader flashbangShader >>= linkShaderProgram . (: [vsp])
      return $ Map.fromList [(Barrel, [barrelProg]), (Stitch, [stitchProg]), (Blur, [blurVProg, blurHProg]), (Flashbang, [flashbangProg])]

data SizeUpdate = GLFWUpdate !(Int, Int) | XUpdate !(CInt, CInt) deriving (Show, Eq)

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
