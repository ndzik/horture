{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

-- Horture
--
-- Initialize fullscreen transparent window without focus GLFW
-- Initialize X11 -> Continuously pull desktop image (excluding GLFW) with x11::GetImage
--                -> Upload image to GPU with OpenGL
--                -> Postprocess and draw to window

import Codec.Picture
import Codec.Picture.Gif
import Control.Monad.Except
import Data.Bits hiding (rotate)
import Data.ByteString (readFile)
import Data.List (elem, find)
import Data.Maybe
import Data.Vector.Storable (toList)
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr
import Graphics.GLUtil as Util hiding (throwError)
import Graphics.GLUtil.Camera2D as Cam2D
import Graphics.GLUtil.Camera3D as Cam3D
import Graphics.Rendering.OpenGL as GL hiding (Color, flush, rotate)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import Horture
import Horture.Effect
import Horture.Horture
import Horture.Object
import Horture.Render
import Horture.Scene
import Horture.State
import Linear.Matrix
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.V4
import System.Clock
import System.Exit
import qualified System.Random as Random
import Text.RawString.QQ
import Prelude hiding (readFile)

main :: IO ()
main =
  x11UserGrabWindow >>= \case
    Nothing -> print "No window to horture yourself on selected 🤨, aborting" >> exitFailure
    Just w -> main' w

main' :: Window -> IO ()
main' w = do
  glW <- initGLFW
  (vao, vbo, veo, prog, gifProg) <- initResources
  dp <- openDisplay ""
  let ds = defaultScreen dp
      cm = defaultColormap dp ds
  root <- rootWindow dp ds

  Just (_, meW) <- findMe root dp hortureName
  allocaSetWindowAttributes $ \ptr -> do
    set_event_mask ptr noEventMask
    changeWindowAttributes dp meW cWEventMask ptr
    set_border_pixel ptr 0
    changeWindowAttributes dp meW cWBorderPixel ptr
  GLFW.setCursorInputMode glW GLFW.CursorInputMode'Hidden
  GLFW.setStickyKeysInputMode glW GLFW.StickyKeysInputMode'Disabled
  GLFW.setStickyMouseButtonsInputMode glW GLFW.StickyMouseButtonsInputMode'Disabled

  attr <- getWindowAttributes dp w
  allocaSetWindowAttributes $ \ptr -> do
    _ <- set_event_mask ptr structureNotifyMask
    changeWindowAttributes dp w cWEventMask ptr
  print "Set window attributes"

  res <- xCompositeQueryExtension dp
  when (isNothing res) $
    print "xCompositeExtension is missing!" >> exitFailure
  print "Queried composite extension"

  -- GIFs
  gifModelUniform <- uniformLocation gifProg "model"
  content <- readFile "/home/omega/Downloads/ricardoflick.gif"
  imgs <- case decodeGifImages content of
    Left err -> print ("decoding ricardogif: " <> err) >> exitFailure
    Right imgs -> return imgs
  delayms <- case getDelaysGifImages content of
    Left err -> print ("getting gif delays: " <> err) >> exitFailure
    Right ds -> return . head $ ds

  -- Set active texture slot.
  currentProgram $= Just gifProg
  texIndex <- uniformLocation gifProg "index"
  uniform texIndex $= (0 :: GLint)

  let gifUnit = TextureUnit 4
  activeTexture $= gifUnit
  texGif <- genObjectName @TextureObject
  textureBinding Texture2DArray $= Just texGif
  let imgsL = concatMap (\(ImageRGBA8 (Codec.Picture.Image _ _ v)) -> toList v) imgs
      (gifW, gifH) = case head imgs of
        ImageRGBA8 (Codec.Picture.Image w h _) -> (w, h)
        _ -> error "decoding gif image"
  withArray imgsL $ \ptr -> do
    let pixelData = PixelData BGRA UnsignedInt8888Rev ptr
    texImage3D
      Texture2DArray
      NoProxy
      0
      RGBA8
      (TextureSize3D (fromIntegral gifW) (fromIntegral gifH) (fromIntegral . length $ imgs))
      0
      pixelData
    generateMipmap' Texture2DArray

  gifTexUni <- uniformLocation gifProg "gifTexture"
  uniform gifTexUni $= gifUnit

  activeTexture $= TextureUnit 0
  currentProgram $= Just prog
  --

  bindVertexArrayObject $= Just vao
  bindBuffer ArrayBuffer $= Just vbo
  withArray verts $ \ptr -> bufferData ArrayBuffer $= (fromIntegral planeVertsSize, ptr, StaticDraw)
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr 0))
  vertexAttribArray (AttribLocation 0) $= Enabled
  vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr (3 * floatSize)))
  vertexAttribArray (AttribLocation 1) $= Enabled
  bindBuffer ElementArrayBuffer $= Just veo
  withArray vertsElement $ \ptr -> bufferData ElementArrayBuffer $= (fromIntegral vertsElementSize, ptr, StaticDraw)

  GL.clearColor $= Color4 0.1 0.1 0.1 1

  screenTexObject <- genObjectName @TextureObject
  -- CompositeRedirectManual to avoid unnecessarily drawing the captured
  -- window, which is overlayed anyway by our application.
  _ <- xCompositeRedirectWindow dp w CompositeRedirectManual
  pm <- xCompositeNameWindowPixmap dp w
  print $ "Retrieved composite window pixmap: " <> show pm

  let ww = wa_width attr
      wh = wa_height attr
  GLFW.setFramebufferSizeCallback glW (Just resizeWindow')

  let !anyPixelData = PixelData BGRA UnsignedByte nullPtr
  textureBinding Texture2D $= Just screenTexObject
  texImage2D
    Texture2D
    NoProxy
    0
    RGBA'
    (TextureSize2D (fromIntegral ww) (fromIntegral wh))
    0
    anyPixelData
  print "Created dummy texture"

  let proj = curry projectionForAspectRatio (fromIntegral ww) (fromIntegral wh)
  projectionUniform <- uniformLocation prog "proj"
  m44ToGLmatrix proj >>= (uniform projectionUniform $=)

  let view = Cam3D.camMatrix (Cam3D.fpsCamera @Float)
  viewUniform <- uniformLocation prog "view"
  m44ToGLmatrix view >>= (uniform viewUniform $=)

  let model = curry scaleForAspectRatio (fromIntegral ww) (fromIntegral wh)
  modelUniform <- uniformLocation prog "model"
  m44ToGLmatrix model >>= (uniform modelUniform $=)

  GLFW.setWindowSize glW (fromIntegral . wa_width $ attr) (fromIntegral . wa_height $ attr)
  GLFW.setWindowPos glW (fromIntegral . wa_x $ attr) (fromIntegral . wa_y $ attr)

  timeUniform <- uniformLocation prog "dt"
  Just startTime <- GLFW.getTime
  uniform timeUniform $= (0 :: Float)

  gifModelUniform <- uniformLocation gifProg "model"
  m44ToGLmatrix identityM44 >>= (uniform gifModelUniform $=)

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  gen <- Random.getStdGen
  let effs = map (\v -> AddGif Ricardo (Limited 8) (V3 (sin (20 * v)) (cos (33 * v)) 0)) . take 10 $ Random.randoms @Float gen
      scene =
        applyAll effs startTime 0 $
          Scene
            { _screen = defScreen,
              _gifs = []
            }
      hs =
        HortureState
          { _display = dp,
            _xWin = w,
            _capture = pm,
            _dim = (fromIntegral . wa_width $ attr, fromIntegral . wa_height $ attr)
          }
      hc =
        HortureStatic
          { _backgroundProg = prog,
            _gifProg = gifProg,
            _modelUniform = modelUniform,
            _viewUniform = viewUniform,
            _projUniform = projectionUniform,
            _planeVertexLocation = AttribLocation 0,
            _planeTexLocation = AttribLocation 1,
            _screenTexUnit = TextureUnit 0,
            _screenTexObject = screenTexObject,
            _gifModelUniform = gifModelUniform,
            _gifTexUnit = gifUnit,
            _gifTexObject = texGif,
            _glWin = glW,
            _gifIndex = texIndex,
            _backgroundColor = Color4 0.1 0.1 0.1 1
          }
  -- TODO: Shutdown xWin when application closes.
  runHorture hs hc (playScene scene)
  print "Done..."

findMe :: Window -> Display -> String -> IO (Maybe ([[Char]], Window))
findMe root dp me = do
  (root, parent, childs) <- queryTree dp root
  alloca $ \ptr -> do
    res <-
      mapM
        ( \c -> do
            xGetTextProperty dp c ptr wM_CLASS
            r <- peek ptr >>= wcTextPropertyToTextList dp
            if not . null $ r
              then print r >> return (Just (r, c))
              else return Nothing
        )
        childs
    return . join
      . find
        ( \case
            (Just (ns, c)) -> hortureName `elem` ns
            _ -> False
        )
      $ res

createGifTex :: DynamicImage -> IO ()
createGifTex (ImageRGBA8 i) = undefined
createGifTex _ = error "unhandled image type encountered when creating GIF texture"
