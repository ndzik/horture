{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- Horture
--
-- Initialize fullscreen transparent window without focus GLFW
-- Initialize X11 -> Continuously pull desktop image (excluding GLFW) with x11::GetImage
--                -> Upload image to GPU with OpenGL
--                -> Postprocess and draw to window

import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.Chan.Synchronous
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.Except
import Data.Bits
import Data.ByteString.Char8 (ByteString, pack)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr
import Graphics.GLUtil as Util hiding (throwError)
import Graphics.Rendering.OpenGL as GL hiding (Color)
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11 hiding (resizeWindow)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import System.Clock
import System.Exit
import Text.RawString.QQ

main :: IO ()
main = findTest

updateTest :: IO ()
updateTest = do
  dp <- openDisplay ""
  let ds = defaultScreen dp
      cm = defaultColormap dp ds
  w <- rootWindow dp ds
  Just w <- findClassName dp w "private"
  attr <- getWindowAttributes dp w
  print $ "Window with ID: " <> show w
  print $ "Initial window dimension: " <> show (wa_width attr, wa_height attr)
  print $ "Window your event mask: " <> (show . wa_your_event_mask $ attr)
  print $ "Window all event mask: " <> (show . wa_all_event_masks $ attr)
  -- NOTE: Need to set the events we are interested in ON the window we are
  -- listening on!
  allocaSetWindowAttributes $ \ptr -> do
    _ <- set_event_mask ptr structureNotifyMask
    changeWindowAttributes dp w cWEventMask ptr

  chan <- newChan @(CInt, CInt)
  forkIO (listenResizeEvent dp w chan)

  print "Trying to read..."
  go dp w chan
  print "Done..."
  where
    go dp w chan = do
      tryReadChan chan >>= \case
        Success newSize -> print newSize
        _ -> return ()
      go dp w chan

findTest :: IO ()
findTest = do
  glW <- initGLFW
  (vao, vbo, veo, prog) <- initResources
  dp <- openDisplay ""
  let ds = defaultScreen dp
      cm = defaultColormap dp ds
  w <- rootWindow dp ds
  Just w <- findClassName dp w "private"
  attr <- getWindowAttributes dp w

  -- NOTE: Need to set the events we are interested in ON the window we are
  -- listening on!
  allocaSetWindowAttributes $ \ptr -> do
    _ <- set_event_mask ptr structureNotifyMask
    changeWindowAttributes dp w cWEventMask ptr

  chan <- newChan @(CInt, CInt)
  forkOS (listenResizeEvent dp w chan)

  -- NOTE: A RUNNING compositemanager is ALSO required!
  res <- xCompositeQueryExtension dp
  when (isNothing res) $
    print "xCompositeExtension is missing!" >> exitFailure

  xCompositeRedirectWindow dp w CompositeRedirectAutomatic

  bindVertexArrayObject $= Just vao
  bindBuffer ArrayBuffer $= Just vbo
  withArray verts $ \ptr -> bufferData ArrayBuffer $= (fromIntegral planeVertsSize, ptr, StaticDraw)
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr 0))
  vertexAttribArray (AttribLocation 0) $= Enabled
  vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr (3 * floatSize)))
  vertexAttribArray (AttribLocation 1) $= Enabled
  bindBuffer ElementArrayBuffer $= Just veo
  withArray vertsElement $ \ptr -> bufferData ElementArrayBuffer $= (fromIntegral vertsElementSize, ptr, StaticDraw)

  tex <- genObjectName @TextureObject
  pm <- xCompositeNameWindowPixmap dp w

  -- Use compositeoverlaywindow for masking:
  --     Window overlay_window = XCompositeGetOverlayWindow(display, root);
  --     Make GLFW Window with proper size and location child of overlay_window
  --     via reparenting.
  --     DONE?!

  let update pm (ww, wh) = do
        start <- getTime Monotonic

        (pm, (ww, wh)) <-
          tryReadChan chan >>= \case
            Success newSize -> do
              print "Window update caught!"
              freePixmap dp pm
              pm <- xCompositeNameWindowPixmap dp w
              return (pm, newSize)
            _ -> return (pm, (ww, wh))

        (glww, glwh) <- GLFW.getWindowSize glW
        i <-
          getImage
            dp
            pm
            0
            0
            (fromIntegral ww)
            (fromIntegral wh)
            0xFFFFFFFF
            -- zPixmap required!
            -- https://stackoverflow.com/questions/34662275/xgetimage-takes-a-lot-of-time-to-run
            zPixmap

        src <- ximageData i
        let pixelData = PixelData BGRA UnsignedByte src
        textureBinding Texture2D $= Just tex
        texImage2D
          Texture2D
          NoProxy
          0
          RGBA'
          (TextureSize2D (fromIntegral ww) (fromIntegral wh))
          0
          pixelData

        generateMipmap' Texture2D
        activeTexture $= TextureUnit 0
        texUniform <- uniformLocation prog "texture1"
        uniform @GLint texUniform $= 0

        destroyImage i

        GL.clearColor $= Color4 0.1 0.1 0.1 1
        GL.clear [ColorBuffer]

        bindVertexArrayObject $= Just vao
        drawElements Triangles 6 UnsignedInt nullPtr

        GLFW.swapBuffers glW
        GLFW.pollEvents

        end <- getTime Monotonic
        let dt = nsec (end - start)
            frameTime = fromIntegral dt / (10 ^ 6)
        print $ "Frametime: " <> show frameTime <> " ms"

        update pm (ww, wh)

  update pm (wa_width attr, wa_height attr)
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

main''' :: IO ()
main''' = do
  glW <- initGLFW
  (vao, vbo, veo, prog) <- initResources
  dp <- openDisplay ""
  let ds = defaultScreen dp
      cm = defaultColormap dp ds
  w <- rootWindow dp ds
  attr <- getWindowAttributes dp w

  bindVertexArrayObject $= Just vao
  bindBuffer ArrayBuffer $= Just vbo
  withArray verts $ \ptr -> bufferData ArrayBuffer $= (fromIntegral planeVertsSize, ptr, StaticDraw)
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr 0))
  vertexAttribArray (AttribLocation 0) $= Enabled
  vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr (3 * floatSize)))
  vertexAttribArray (AttribLocation 1) $= Enabled
  bindBuffer ElementArrayBuffer $= Just veo
  withArray vertsElement $ \ptr -> bufferData ElementArrayBuffer $= (fromIntegral vertsElementSize, ptr, StaticDraw)

  let ww = fromIntegral $ wa_width attr
      wh = fromIntegral $ wa_height attr
      update = do
        (glww, glwh) <- GLFW.getWindowSize glW
        i <-
          getImage
            dp
            w
            0
            0
            (fromIntegral ww)
            (fromIntegral wh)
            0xFFFFFFFF
            -- zPixmap required!
            -- https://stackoverflow.com/questions/34662275/xgetimage-takes-a-lot-of-time-to-run
            zPixmap

        src <- ximageData i
        let pixelData = PixelData BGRA UnsignedByte src

        tex <- genObjectName @TextureObject
        textureBinding Texture2D $= Just tex
        texImage2D
          Texture2D
          NoProxy
          0
          RGBA'
          (TextureSize2D (fromIntegral ww) (fromIntegral wh))
          0
          pixelData

        generateMipmap' Texture2D
        activeTexture $= TextureUnit 0
        texUniform <- uniformLocation prog "texture1"
        uniform @GLint texUniform $= 0

        destroyImage i

        GL.clearColor $= Color4 0.1 0.1 0.1 1
        GL.clear [ColorBuffer]

        bindVertexArrayObject $= Just vao
        drawElements Triangles 6 UnsignedInt nullPtr

        GLFW.swapBuffers glW
        GLFW.pollEvents
        update

  update
  print "Done..."

main' :: IO ()
main' = do
  glW <- initGLFW
  (vao, vbo, veo, prog) <- initResources
  dp <- openDisplay ""
  let ds = defaultScreen dp
      cm = defaultColormap dp ds
  w <- rootWindow dp ds
  attr <- getWindowAttributes dp w

  bindVertexArrayObject $= Just vao
  bindBuffer ArrayBuffer $= Just vbo
  withArray verts $ \ptr -> bufferData ArrayBuffer $= (fromIntegral planeVertsSize, ptr, StaticDraw)
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr 0))
  vertexAttribArray (AttribLocation 0) $= Enabled
  vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr (3 * floatSize)))
  vertexAttribArray (AttribLocation 1) $= Enabled
  bindBuffer ElementArrayBuffer $= Just veo
  withArray vertsElement $ \ptr -> bufferData ElementArrayBuffer $= (fromIntegral vertsElementSize, ptr, StaticDraw)

  let ww = fromIntegral $ wa_width attr
      wh = fromIntegral $ wa_height attr
      update = do
        (glww, glwh) <- GLFW.getWindowSize glW
        i <-
          getImage
            dp
            w
            0
            0
            (fromIntegral ww)
            (fromIntegral wh)
            0xFFFFFFFF
            -- zPixmap required!
            -- https://stackoverflow.com/questions/34662275/xgetimage-takes-a-lot-of-time-to-run
            zPixmap

        -- TODO: Make efficient...
        let sz = ww * wh
        src <- mallocArray @Word64 $ ww * wh
        iterate' (<= sz) $ \idx ->
          let x = fromIntegral $ idx `mod` ww
              y = fromIntegral $ idx `div` ww
              px = getPixel i x y
           in queryColor dp cm (Color (fromIntegral px) 0 0 0 0)
                >>= \(Color _ r g b _) -> do
                  pokeByteOff @Word16 src (idx * 8 + 0) r
                  pokeByteOff @Word16 src (idx * 8 + 2) g
                  pokeByteOff @Word16 src (idx * 8 + 4) b
                  pokeByteOff @Word16 src (idx * 8 + 6) 0xffff

        let pixelData = PixelData RGBA UnsignedShort src

        tex <- genObjectName @TextureObject
        textureBinding Texture2D $= Just tex
        texImage2D
          Texture2D
          NoProxy
          0
          RGB'
          (TextureSize2D (fromIntegral ww) (fromIntegral wh))
          0
          pixelData

        generateMipmap' Texture2D
        activeTexture $= TextureUnit 0
        texUniform <- uniformLocation prog "texture1"
        uniform @GLint texUniform $= 0

        free src
        destroyImage i

        GL.clearColor $= Color4 0.1 0.1 0.1 1
        GL.clear [ColorBuffer]

        bindVertexArrayObject $= Just vao
        drawElements Triangles 6 UnsignedInt nullPtr

        GLFW.swapBuffers glW
        GLFW.pollEvents
        update

  update
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
  GLFW.init
  GLFW.defaultWindowHints
  win <-
    GLFW.createWindow 512 512 "Horture" Nothing Nothing >>= \case
      Nothing -> throwError . userError $ "Failed to create GLFW window"
      Just win -> return win
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowSizeCallback win (Just resizeWindow)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  return win

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed win GLFW.Key'Q _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _ _ _ _ _ = return ()

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

listenResizeEvent :: Display -> Window -> Chan (CInt, CInt) -> IO ()
listenResizeEvent dp w c = go
  where
    go = do
      allocaXEvent $ \evptr -> do
        doIt <- checkWindowEvent dp w structureNotifyMask evptr
        when doIt $ do
          w' <- get_Window evptr
          getEvent evptr >>= \case
            ConfigureEvent {..} -> writeChan c (ev_width, ev_height)
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
        xs@(_ : _) -> return . last $ xs
        _ -> return Nothing
