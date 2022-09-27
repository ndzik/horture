{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Horture
  ( SizeUpdate (..),
    hortureName,
    listenResizeEvent,
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
    scaleForAspectRatio,
    projectionForAspectRatio,
    degToRad,
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
import Data.IORef
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLUtil as Util hiding (throwError)
import Graphics.GLUtil.Camera3D as Util
import Graphics.Rendering.OpenGL as GL hiding (Color, flush)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11 hiding (resizeWindow)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import Horture.Error
import Horture.Horture
import Horture.Render
import Horture.Scene
import Horture.State
import Linear.Matrix
import Linear.V4
import System.Exit
import Text.RawString.QQ

playScene :: Scene -> Horture ()
playScene s = do
  startTime <- getTime
  go startTime s
  where
    go startTime s = do
      dt <- deltaTime startTime
      clearView
      renderScreen dt . _screen $ s
      renderObjects dt . _gifs $ s
      updateView
      pollEvents
      s' <- getTime <&> flip purge s
      go startTime s'

clearView :: Horture ()
clearView = liftIO $ GL.clear [ColorBuffer]

updateView :: Horture ()
updateView = asks _glWin >>= liftIO . GLFW.swapBuffers

pollEvents :: Horture ()
pollEvents = do
  liftIO GLFW.pollEvents
  glWin <- asks _glWin
  projectionUniform <- asks _projUniform
  modelUniform <- asks _modelUniform
  screenTexObj <- asks _screenTexObject
  screenTexUnit <- asks _screenTexUnit
  xWin <- gets _xWin
  dp <- gets _display
  pm <- gets _capture
  (ww, wh) <- gets _dim
  (pm, (ww, wh)) <- liftIO $
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
              let glw = fromIntegral ev_width
                  glh = fromIntegral ev_height
              GLFW.setWindowSize glWin glw glh
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

              let proj = curry projectionForAspectRatio (fromIntegral ww) (fromIntegral wh)
              m44ToGLmatrix proj >>= (uniform projectionUniform $=)

              let model = curry scaleForAspectRatio (fromIntegral ww) (fromIntegral wh)
              m44ToGLmatrix model >>= (uniform modelUniform $=)

              return (newPm, (fromIntegral ev_width, fromIntegral ev_height))
            _ -> return (pm, (ww, wh))
        else return (pm, (ww, wh))
  modify $ \hs -> hs {_dim = (ww, wh), _capture = pm}

deltaTime :: Double -> Horture Double
deltaTime startTime =
  getTime >>= \currentTime -> return $ currentTime - startTime

getTime :: Horture Double
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
  unless i $ print "GLFW.init failed" >> exitFailure

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
  frag_colour = vec4(colour.x, colour.y, colour.z, colour.w);
}
    |]

data SizeUpdate = GLFWUpdate (Int, Int) | XUpdate (CInt, CInt) deriving (Show, Eq)

x11UserGrabWindow :: IO (Maybe Window)
x11UserGrabWindow = do
  print "opening display"
  dp <- openDisplay ""
  let ds = defaultScreen dp
  print "getting root window"
  root <- rootWindow dp ds
  print "creating cursor"
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
            then print "Unabled to fetch name of grabbed window"
            else do
              s <- peek cptr >>= peekCString
              print $ "Grabbing window: " <> s
        return . Just $ ev_subwindow
      _ -> return Nothing

  ungrabPointer dp currentTime
  freeCursor dp cursor
  closeDisplay dp
  return userDecision

listenResizeEvent :: Display -> Window -> GLFW.Window -> IORef (Drawable, (CInt, CInt)) -> IO ()
listenResizeEvent dp w glW r = go
  where
    go = do
      allocaXEvent $ \evptr -> do
        doIt <- checkWindowEvent dp w structureNotifyMask evptr
        when doIt $ do
          getEvent evptr >>= \case
            ConfigureEvent {..} -> do
              -- Retrieve a new pixmap.
              pm <- xCompositeNameWindowPixmap dp w
              -- Update reference, aspect ratio & destroy old pixmap.
              pm <- atomicModifyIORef' r $ \(d, _) -> ((pm, (ev_width, ev_height)), d)
              freePixmap dp pm
              -- Update overlay window with new aspect ratio.
              let w = fromIntegral ev_width
                  h = fromIntegral ev_height
              GLFW.setWindowSize glW w h
            _ -> return ()
      go

hortureName :: String
hortureName = "horture"

identityM44 :: M44 Float
identityM44 =
  V4
    (V4 1 0 0 0)
    (V4 0 1 0 0)
    (V4 0 0 1 0)
    (V4 0 0 0 1)

m44ToGLmatrix :: (Show a, MatrixComponent a) => M44 a -> IO (GLmatrix a)
m44ToGLmatrix m = withNewMatrix ColumnMajor (\p -> poke (castPtr p) m')
  where
    m' = transpose m

degToRad :: Float -> Float
degToRad = (*) (pi / 180)

scaleForAspectRatio :: (Float, Float) -> M44 Float
scaleForAspectRatio (ww, wh) = model
  where
    ident = identityM44
    aspectRatio = ww / wh
    scaling =
      V4
        (V4 aspectRatio 0 0 0)
        (V4 0 1 0 0)
        (V4 0 0 1 0)
        (V4 0 0 0 1)
    model = scaling !*! ident

projectionForAspectRatio :: (Float, Float) -> M44 Float
projectionForAspectRatio (ww, wh) = proj
  where
    proj = Util.projectionMatrix (degToRad 90) (ww / wh) 1 100
