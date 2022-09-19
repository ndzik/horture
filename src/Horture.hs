{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Horture
  ( SizeUpdate (..),
    hortureName,
    listenResizeEvent,
    resizeWindow,
    verts,
    floatSize,
    planeVertsSize,
    vertsElement,
    vertsElementSize,
    ximageData,
    initResources,
    initGLFW,
  )
where

import Control.Monad.Except
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
import Graphics.Rendering.OpenGL as GL hiding (Color, flush)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11 hiding (resizeWindow)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import System.Clock
import System.Exit
import Text.RawString.QQ

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

  start <- getTime Monotonic
  r <- loop 0
  end <- getTime Monotonic

  let dt = nsec (end - start)
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
  win <-
    GLFW.createWindow 1024 1024 hortureName Nothing Nothing >>= \case
      Nothing -> throwError . userError $ "Failed to create GLFW window"
      Just win -> return win
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowCloseCallback win (Just shutdown)

  return win

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow _ w h = do
  -- TODO: GLFW.resizeWindow here?
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

initResources :: IO (VertexArrayObject, BufferObject, BufferObject, Program)
initResources = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  vsp <- loadShaderBS "vertex.shader" VertexShader hortureVertexShader
  fsp <- loadShaderBS "fragment.shader" FragmentShader hortureFragmentShader
  prog <- linkShaderProgram [vsp, fsp]
  currentProgram $= Just prog
  veo <- genObjectName
  bindBuffer ArrayBuffer $= Just veo
  return (vao, vbo, veo, prog)

iterate' :: Monad m => (Int -> Bool) -> (Int -> m ()) -> m ()
iterate' b f = go 0 b f
  where
    go :: Monad m => Int -> (Int -> Bool) -> (Int -> m ()) -> m ()
    go n b f = when (b n) $ f n >> go (n + 1) b f

hortureVertexShader :: ByteString
hortureVertexShader =
  pack
    [r|
#version 410

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 texCoord;

void main() {
  gl_Position = vec4(aPos, 1.0);
  texCoord = aTexCoord;
}
   |]

hortureFragmentShader :: ByteString
hortureFragmentShader =
  pack
    [r|
#version 410

in vec2 texCoord;

uniform sampler2D texture1;

out vec4 frag_colour;

void main() {
  frag_colour = texture(texture1, texCoord);
}
    |]

data SizeUpdate = GLFWUpdate (Int, Int) | XUpdate (CInt, CInt) deriving (Show, Eq)

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
