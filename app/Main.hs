{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- Horture
--
-- Initialize fullscreen transparent window without focus GLFW
-- Initialize X11 -> Continuously pull desktop image (excluding GLFW) with x11::GetImage
--                -> Upload image to GPU with OpenGL
--                -> Postprocess and draw to window

import Control.Monad.Except
import Data.ByteString.Char8 (ByteString, pack)
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLUtil as Util hiding (throwError)
import Graphics.Rendering.OpenGL as GL hiding (Color)
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11 hiding (resizeWindow)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import System.Exit
import Text.RawString.QQ

main :: IO ()
main = do
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

  let ww = 512 -- wa_width attr
      wh = 512 -- wa_height attr
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
            xyPixmap

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
