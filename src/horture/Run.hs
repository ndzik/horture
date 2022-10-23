{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Run
  ( run,
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Lens
import Control.Monad.Except
import Data.Default
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLUtil.Camera3D as Cam3D
import Graphics.Rendering.OpenGL as GL hiding (Color, flush, rotate)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.X11
import Graphics.X11.Xlib.Extras hiding (Event)
import Horture
import Horture.Event
import Horture.Horture
import Horture.Loader
import Horture.Loader.Asset
import Horture.Program
import Horture.Render
import Horture.Scene
import Horture.State
import System.Exit
import Prelude hiding (readFile)

run :: [(FilePath, Asset)] -> Maybe (Chan Text) -> Chan Event -> Window -> IO ()
run gifs logChan evChan w = do
  glW <- initGLFW
  (vao, vbo, veo, prog, gifProg, effs) <- initResources
  dp <- openDisplay ""
  let ds = defaultScreen dp
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

  res <- xCompositeQueryExtension dp
  when (isNothing res) exitFailure

  let screenTextureUnit = TextureUnit 0
      gifTextureUnit = TextureUnit 4

  -- GIFs
  gifModelUniform <- uniformLocation gifProg "model"
  gifTexUni <- uniformLocation gifProg "gifTexture"
  gifTexIndex <- uniformLocation gifProg "index"
  (loaderResult, loaderState) <-
    runTextureLoader
      ( LC
          { _loaderConfigPreloadedGifs = gifs,
            _loaderConfigGifProg = gifProg,
            _loaderConfigGifTexUniform = gifTexUni,
            _loaderConfigGifTextureUnit = gifTextureUnit
          }
      )
      def
      loadGifsGL
  hortureGifs <- case loaderResult of
    Left _ -> exitFailure
    _otherwise -> do
      return $ loaderState ^. resolvedGifs
  activeTexture $= screenTextureUnit
  currentProgram $= Just prog
  --

  let vertexAttributeLocation = AttribLocation 0
      texAttributeLocation = AttribLocation 1
  bindVertexArrayObject $= Just vao
  bindBuffer ArrayBuffer $= Just vbo
  withArray verts $ \ptr -> bufferData ArrayBuffer $= (fromIntegral planeVertsSize, ptr, StaticDraw)
  vertexAttribPointer vertexAttributeLocation $= (ToFloat, VertexArrayDescriptor 3 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr 0))
  vertexAttribArray vertexAttributeLocation $= Enabled
  vertexAttribPointer texAttributeLocation $= (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral $ 5 * floatSize) (plusPtr nullPtr (3 * floatSize)))
  vertexAttribArray texAttributeLocation $= Enabled
  bindBuffer ElementArrayBuffer $= Just veo
  withArray vertsElement $ \ptr -> bufferData ElementArrayBuffer $= (fromIntegral vertsElementSize, ptr, StaticDraw)

  GL.clearColor $= Color4 0.1 0.1 0.1 1

  -- CompositeRedirectManual to avoid unnecessarily drawing the captured
  -- window, which is overlayed anyway by our application.
  _ <- xCompositeRedirectWindow dp w CompositeRedirectManual
  pm <- xCompositeNameWindowPixmap dp w

  let ww = wa_width attr
      wh = wa_height attr
  GLFW.setFramebufferSizeCallback glW (Just resizeWindow')

  -- Initialize source texture holding captured window image.
  backTexture <- genObjectName
  let !anyPixelData = PixelData BGRA UnsignedByte nullPtr
  textureBinding Texture2D $= Just backTexture
  texImage2D
    Texture2D
    NoProxy
    0
    RGBA'
    (TextureSize2D (fromIntegral ww) (fromIntegral wh))
    0
    anyPixelData
  renderedTexture <- genObjectName
  textureBinding Texture2D $= Just renderedTexture
  texImage2D
    Texture2D
    NoProxy
    0
    RGBA'
    (TextureSize2D (fromIntegral ww) (fromIntegral wh))
    0
    anyPixelData

  -- FRAMEBUFFER SETUP BEGIN
  -- fb is the framebuffer, grouping our textures.
  fb <- genObjectName
  bindFramebuffer Framebuffer $= fb

  -- Configure framebuffer. We will directly bind our source texture as a
  -- colorattachment.
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D renderedTexture 0
  drawBuffers $= [FBOColorAttachment 0]
  -- FRAMEBUFFER SETUP END

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
  uniform timeUniform $= (0 :: Float)

  m44ToGLmatrix identityM44 >>= (uniform gifModelUniform $=)

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  let scene =
        def
          { _screen = def,
            _gifs = Map.empty,
            _gifCache = hortureGifs
          }
      hs =
        HortureState
          { _display = dp,
            _xWin = w,
            _capture = pm,
            _dim = (fromIntegral . wa_width $ attr, fromIntegral . wa_height $ attr)
          }
  let hc =
        HortureStatic
          { _screenProg =
              HortureScreenProgram
                { _hortureScreenProgramShader = prog,
                  _hortureScreenProgramShaderEffects = effs,
                  _hortureScreenProgramModelUniform = modelUniform,
                  _hortureScreenProgramProjectionUniform = projectionUniform,
                  _hortureScreenProgramViewUniform = viewUniform,
                  _hortureScreenProgramTimeUniform = timeUniform,
                  _hortureScreenProgramFramebuffer = fb,
                  _hortureScreenProgramTextureObject = renderedTexture,
                  _hortureScreenProgramBackTextureObject = backTexture,
                  _hortureScreenProgramTextureUnit = screenTextureUnit
                },
            _gifProg =
              HortureGifProgram
                { _hortureGifProgramShader = gifProg,
                  _hortureGifProgramModelUniform = gifModelUniform,
                  _hortureGifProgramIndexUniform = gifTexIndex,
                  _hortureGifProgramTextureUnit = gifTextureUnit,
                  _hortureGifProgramAssets = hortureGifs
                },
            _eventChan = evChan,
            _logChan = logChan,
            _planeVertexLocation = vertexAttributeLocation,
            _planeTexLocation = texAttributeLocation,
            _glWin = glW,
            _backgroundColor = Color4 0.1 0.1 0.1 1
          }
  _ <- case logChan of
    Just _ -> runHorture hs hc (playScene @'Channel scene)
    Nothing -> runHorture hs hc (playScene @'NoLog scene)
  GLFW.destroyWindow glW
  GLFW.terminate
  closeDisplay dp

findMe :: Window -> Display -> String -> IO (Maybe ([[Char]], Window))
findMe root dp me = do
  (_root, _parent, childs) <- queryTree dp root
  alloca $ \ptr -> do
    res <-
      mapM
        ( \c -> do
            _ <- xGetTextProperty dp c ptr wM_CLASS
            r <- peek ptr >>= wcTextPropertyToTextList dp
            if not . null $ r
              then return (Just (r, c))
              else return Nothing
        )
        childs
    return . join
      . find
        ( \case
            (Just (ns, _c)) -> me `elem` ns
            _otherwise -> False
        )
      $ res
