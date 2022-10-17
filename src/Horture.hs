{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
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

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.ByteString.Char8 (ByteString, pack)
import Data.Functor ((<&>))
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
import Horture.Error
import Horture.Events
import Horture.Horture
import Horture.Logging
import Horture.Render
import Horture.Scene
import Horture.Shaders
import Horture.State
import System.Exit
import Text.RawString.QQ

hortureName :: String
hortureName = "horture"

playScene :: (HortureLogger (Horture l)) => Scene -> Horture l ()
playScene s = do
  setTime 0
  go 0 (Just s)
  where
    go _ Nothing = return ()
    go startTime (Just s) = do
      dt <- deltaTime startTime
      clearView
      renderScreen dt . _screen $ s
      renderGifs dt . _gifs $ s
      updateView
      s' <- getTime >>= \timeNow -> pollEvents s timeNow dt <&> (purge timeNow <$>)
      go startTime s'

clearView :: Horture l ()
clearView = liftIO $ GL.clear [ColorBuffer]

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
  projectionUniform <- asks _projUniform
  modelUniform <- asks _modelUniform
  screenTexObj <- asks _screenTexObject
  screenTexUnit <- asks _screenTexUnit
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

              let proj = curry projectionForAspectRatio newWFloat newHFloat
              m44ToGLmatrix proj >>= (uniform projectionUniform $=)

              let model = curry scaleForAspectRatio newWInt newHInt
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

initResources :: IO (VertexArrayObject, BufferObject, BufferObject, Program, Program)
initResources = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  vsp <- loadShaderBS "vertex.shader" VertexShader hortureVertexShader
  fsp <- loadShaderBS "fragment.shader" FragmentShader hortureFragmentShader
  prog <- linkShaderProgram [vsp, fsp]
  vspg <- loadShaderBS "gifvertex.shader" VertexShader hortureVertexGIF
  fspg <- loadShaderBS "giffragment.shader" FragmentShader hortureFragmentGIF
  gifProg <- linkShaderProgram [vspg, fspg]
  currentProgram $= Just prog
  veo <- genObjectName
  bindBuffer ArrayBuffer $= Just veo
  return (vao, vbo, veo, prog, gifProg)

hortureVertexGIF :: ByteString
hortureVertexGIF =
  pack
    [r|
#version 410

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
uniform float dt;
uniform mat4 model;

out vec2 texCoord;

void main() {
  gl_Position = model * vec4(aPos.x, aPos.y, aPos.z, 1.0);
  texCoord = aTexCoord;
}
   |]

hortureFragmentGIF :: ByteString
hortureFragmentGIF =
  pack
    [r|
#version 410

in vec2 texCoord;

uniform int index;
uniform sampler2DArray gifTexture;

out vec4 frag_colour;

void main() {
  frag_colour = texture(gifTexture, vec3(texCoord.x, texCoord.y, index));
}
    |]

hortureVertexShader :: ByteString
hortureVertexShader =
  pack
    [r|
#version 410

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform float dt;

out vec2 texCoord;

void main() {
  gl_Position = proj * view * model * vec4(aPos.x, aPos.y, aPos.z, 1.0);
  texCoord = aTexCoord;
}
   |]

hortureFragmentShader :: ByteString
hortureFragmentShader =
  pack
    [r|
#version 410

in vec2 texCoord;

uniform float dt;
uniform sampler2D texture1;

out vec4 frag_colour;

void main() {
  vec4 colour = texture(texture1, texCoord);
  // frag_colour = vec4(colour.x+sin(4*texCoord.x+dt), colour.y+cos(12*texCoord.y+dt), colour.z-sin(3*dt), colour.w);
  frag_colour = vec4(colour.x, colour.y, colour.z, 1);
}
    |]

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
