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
import Data.Bits
import Data.ByteString.Char8 (ByteString, pack)
import Data.Functor ((<&>))
import Data.IORef
import Data.List (find)
import Data.Word
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
import Linear.Matrix
import Linear.V4
import qualified System.Clock as Clock
import System.Exit
import Text.RawString.QQ

playScene :: Scene -> Horture ()
playScene s = do
  startTime <- getTime
  go startTime s
  where
    go startTime s = do
      dt <- deltaTime startTime
      timeNow <- getTime
      renderScreen dt . _screen $ s
      renderObjects dt . _gifs $ s
      let s' = purge timeNow s
      pollEvents
      go startTime s'

pollEvents :: Horture ()
pollEvents = undefined

--       (pm, (ww, wh)) <- allocaXEvent $ \evptr -> do
--         doIt <- checkWindowEvent dp w structureNotifyMask evptr
--         if doIt
--           then do
--             getEvent evptr >>= \case
--               ConfigureEvent {..} -> do
--                 -- Retrieve a new pixmap.
--                 newPm <- xCompositeNameWindowPixmap dp w
--                 -- Update reference, aspect ratio & destroy old pixmap.
--                 freePixmap dp pm
--                 -- Update overlay window with new aspect ratio.
--                 let glw = fromIntegral ev_width
--                     glh = fromIntegral ev_height
--                 GLFW.setWindowSize glW glw glh
--                 attr <- getWindowAttributes dp w
--                 GLFW.setWindowPos glW (fromIntegral ev_x) (fromIntegral ev_y)
--                 (glW, glH) <- GLFW.getFramebufferSize glW
--                 let !anyPixelData = PixelData BGRA UnsignedInt8888Rev nullPtr
--                 -- Update texture bindings!
--                 textureBinding Texture2D $= Just tex01
--                 texImage2D
--                   Texture2D
--                   NoProxy
--                   0
--                   RGBA'
--                   (TextureSize2D (fromIntegral ev_width) (fromIntegral ev_height))
--                   0
--                   anyPixelData

--                 let proj = curry projectionForAspectRatio (fromIntegral ww) (fromIntegral wh)
--                 m44ToGLmatrix proj >>= (uniform projectionUniform $=)

--                 let model = curry scaleForAspectRatio (fromIntegral ww) (fromIntegral wh)
--                 m44ToGLmatrix model >>= (uniform modelUniform $=)

--                 return (newPm, (ev_width, ev_height))
--               _ -> return (pm, (ww, wh))
--           else return (pm, (ww, wh))

deltaTime :: Double -> Horture Double
deltaTime startTime =
  getTime >>= \currentTime -> return $ currentTime - startTime

getTime :: Horture Double
getTime =
  liftIO GLFW.getTime >>= \case
    Nothing -> throwError . HE $ "GLFW not running or initialized"
    Just t -> return t

main'' :: IO ()
main'' = do
  dp <- openDisplay ""
  let ds = defaultScreen dp
      cm = defaultColormap dp ds
  w <- rootWindow dp ds
  attr <- getWindowAttributes dp w

  let ww = fromIntegral $ wa_width attr
      wh = fromIntegral $ wa_height attr
      n = 6
      loop :: Int -> IO Int
      loop i = do
        !img <-
          getImage
            dp
            w
            0
            0
            (fromIntegral ww)
            (fromIntegral wh)
            0xFFFFFFFF
            zPixmap

        ximageBytesPerLine img >>= \m -> print $ "BytesPerLine: " <> show m
        ximageOffset img >>= \m -> print $ "Offset: " <> show m
        ximagebitsPerPixel img >>= \m -> print $ "BitsPerPixel: " <> show m

        -- GetPixel query.
        let x = 1
            y = 0
            px = getPixel img x y
        queryColor dp cm (Color (fromIntegral px) 0 0 0 0)
          >>= \(Color _ r g b _) -> print $ "GetPixel: " <> show (r, g, b)

        -- Custom query.
        buf <- ximageData img
        let stride = 4
            -- 3840x1080
            idx = 3840 * 1080 * stride
        peekByteOff @Word8 buf (idx + 0)
          >>= \r ->
            peekByteOff @Word8 buf (idx + 4)
              >>= \g ->
                peekByteOff @Word8 buf (idx + 8)
                  >>= \b -> print $ "Image Inspect: " <> show (r, g, b)
        if i < n then loop (i + 1) else return i

  start <- Clock.getTime Clock.Monotonic
  r <- loop 0
  end <- Clock.getTime Clock.Monotonic

  let dt = Clock.nsec (end - start)
      frameTime = fromIntegral dt / fromIntegral n

  print $ "r: " <> show r
  print $ "dt: " <> show dt
  print $ "n: " <> show n
  print $ "FrameTime (ms): " <> show (frameTime / (10 ^ 6))

  print "Done..."

xtest :: IO ()
xtest = do
  dp <- openDisplay ""
  let ds = defaultScreen dp
      cm = defaultColormap dp ds
  w <- rootWindow dp ds
  (root, parent, childs) <- queryTree dp w
  res <-
    mapM
      ( \c ->
          fetchName dp c >>= \case
            Nothing -> return ("", c)
            Just w -> return (w, c)
      )
      childs
      <&> filter ((/=) "" . fst)

  alloca $ \ptr -> do
    mapM
      ( \c -> do
          s <- xGetTextProperty dp c ptr wM_CLASS
          when (s /= 0) $ peek ptr >>= wcTextPropertyToTextList dp >>= print
      )
      childs
  print "Done..."

verts :: [Float]
verts = [-1, -1, -1, 0, 1, -1, 1, -1, 0, 0, 1, 1, -1, 1, 0, 1, -1, -1, 1, 1]

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

imgPtr :: Image -> Ptr Image
imgPtr (Image p) = p

szCInt :: Int
szCInt = sizeOf @CInt undefined

ximageData :: Image -> IO (Ptr Word8)
ximageData (Image p) = peek (plusPtr @Image @(Ptr CIntPtr) p xdataPtr) >>= \buf -> return . castPtr $ buf
  where
    szCint = sizeOf @CInt undefined
    xdataPtr = 4 * szCint

ximagebitsPerPixel :: Image -> IO CInt
ximagebitsPerPixel (Image p) = peekByteOff @CInt (castPtr p) xbitsPerPixel
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xbitsPerPixel = 4 * szCint + szPtr + 6 * szCint

ximageBytesPerLine :: Image -> IO CInt
ximageBytesPerLine (Image p) = peekByteOff @CInt (castPtr p) xbytesPerLine
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xbytesPerLine = 4 * szCint + szPtr + 5 * szCint

ximageOffset :: Image -> IO CInt
ximageOffset (Image p) = peekByteOff @CInt (castPtr p) xoffset
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xwidth = 0
    xheight = szCint
    xoffset = 2 * szCint
    xformat = 3 * szCint
    xdataPtr = 4 * szCint
    xbyteorder = 4 * szCint + szPtr
    xbitmapUnit = 4 * szCint + szPtr + szCint
    xbitmapBitOrder = 4 * szCint + szPtr + 2 * szCint
    xbitmapPad = 4 * szCint + szPtr + 3 * szCint
    xdepth = 4 * szCint + szPtr + 4 * szCint
    xbytesPerLine = 4 * szCint + szPtr + 5 * szCint
    xbitsPerPixel = 4 * szCint + szPtr + 6 * szCint

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

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed win GLFW.Key'Q _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _ _ _ _ _ = return ()

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
  frag_colour = vec4(colour.x, colour.y, colour.z, colour.w-sin(4*dt));
}
    |]

data SizeUpdate = GLFWUpdate (Int, Int) | XUpdate (CInt, CInt) deriving (Show, Eq)

x11UserGrabWindow :: IO (Maybe Window)
x11UserGrabWindow = do
  dp <- openDisplay ""
  let ds = defaultScreen dp
  root <- rootWindow dp ds
  cursor <- createFontCursor dp xC_crosshair
  grabPointer dp root False (buttonMotionMask .|. buttonPressMask .|. buttonReleaseMask) grabModeAsync grabModeAsync root cursor currentTime

  userDecision <- allocaXEvent $ \evptr -> do
    nextEvent dp evptr
    getEvent evptr >>= \case
      ButtonEvent {..} -> return . Just $ ev_subwindow
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

findClassName :: Display -> Window -> String -> IO (Maybe Window)
findClassName dp w n = do
  (root, parent, childs) <- queryTree dp w
  alloca $ \ptr -> do
    res <-
      mapM
        ( \c -> do
            s <- xGetTextProperty dp c ptr wM_CLASS
            if s /= 0
              then
                peek ptr
                  >>= wcTextPropertyToTextList dp
                  >>= \ss ->
                    case find (n ==) ss of
                      Nothing -> return (Nothing @Window)
                      Just _ -> print ss >> return (Just c)
              else return (Nothing @Window)
        )
        childs
    case res of
      [] -> return Nothing
      fs -> case filter (/= Nothing) fs of
        -- Xlib how to keep drawing unfocused window:
        -- https://linux.die.net/man/3/xcompositenamewindowpixmap
        xs@(_ : _) -> print xs >> return (last xs)
        _ -> return Nothing

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
