{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Horture
  ( SizeUpdate (..),
    HortureEffects,
    hortureName,
    resizeWindow',
    verts,
    floatSize,
    planeVertsSize,
    vertsElement,
    vertsElementSize,
    initResources,
    initGLFW,
    playScene,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (unless, void, when)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import Data.Default
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLUtil as Util hiding (throwError)
import Graphics.GLUtil.Camera3D as Cam3D
import Graphics.Rendering.OpenGL as GL hiding (Color, Invert, flush)
import qualified Graphics.UI.GLFW as GLFW
import Horture.Audio
import Horture.Audio.Player.Horture ()
import Horture.Audio.Recorder.Horture ()
import Horture.Constraints
import Horture.Debug
import Horture.Effect
import Horture.Error
import Horture.Events
import Horture.GL
import Horture.Horture
import Horture.Loader
import Horture.Loader.Asset (Asset)
import Horture.Logging
import Horture.Program
import Horture.Render
import Horture.Scene
import Horture.Shader.Shader
import Horture.State
import Horture.WindowGrabber
import System.Exit

hortureName :: String
hortureName = "horture"

frameTime :: Float
frameTime = 1 / 144 -- target 144 FPS upper limit

-- | playScene plays the given scene in a Horture context.
playScene :: forall m l hdl. (HortureEffects m hdl l) => Scene -> Horture m l hdl ()
playScene s = do
  setTime 0
  void . withAudio . withRecording . go 0 . Just $ s
  where
    go _ Nothing = do
      logInfo "horture stopped"
    go lt (Just s) = do
      let action = do
            (_, timeSince) <- deltaTime 0
            clearView
            fft <- calcCurrentFFTPeak
            _ <- timeCPU "Render background" $ renderBackground lt
            s <- timeCPU "Render whole scene" $ renderScene timeSince fft s
            _ <- timeCPU "Render assets" $ renderAssets timeSince . _assets $ s
            _ <- timeCPU "Render active text" $ renderActiveEffectText s
            _ <- timeCPU "Render event list" $ renderEventList timeSince
            _ <- timeCPU "Update view" $ updateView
            _ <- timeCPU "Counting frame" $ countFrame
            timeNow <- timeCPU "Getting time" $ getTime
            (timeCPU "Polling events" $ pollEvents s timeNow timeSince) >>= processAudio <&> (purge timeNow <$>)
      s <-
        timeCPU "Horture action" $
          action
            `catchError` ( \err -> do
                             handleHortureError err
                             logWarn "resetting scene & continuing..."
                             return $ Just s
                         )
      deltaTime lt >>= \(timeSinceFrame, _) ->
        when (timeSinceFrame < frameTime) $
          liftIO . threadDelay . round $
            (frameTime - timeSinceFrame) * 1000 * 1000
      newTime <- getTime
      go newTime s
    handleHortureError (HE err) = logError . pack $ err
    handleHortureError (WindowEnvironmentInitializationErr err) = logError . pack $ err
    handleHortureError WindowEnvironmentQueryHortureErr = logError . pack . show $ WindowEnvironmentQueryHortureErr
    handleHortureError ase@(AudioSinkUnavailableErr _) = logError . pack . show $ ase
    handleHortureError asi@AudioSinkInitializationErr = logError . pack . show $ asi
    handleHortureError asp@(AudioSinkPlayErr _) = logError . pack . show $ asp

calcCurrentFFTPeak :: (HortureLogger (Horture m l hdl), AudioRecorder (Horture m l hdl)) => Horture m l hdl (Float, Float, Float)
calcCurrentFFTPeak = do
  (b, m, h) <- currentFFTPeak
  absTvar <- asks (^. audioBandState)
  abs <- liftIO . readTVarIO $ absTvar
  let (bNorm, bst') = stepBand (realToFrac b) $ abs ^. abBass
  let (mNorm, mst') = stepBand (realToFrac m) $ abs ^. abMid
  let (hNorm, hst') = stepBand (realToFrac h) $ abs ^. abHigh
  liftIO . atomically . writeTVar absTvar $ (AllBands {_abMid = mst', _abHigh = hst', _abBass = bst'})
  return (bNorm, mNorm, hNorm)

-- stepBand: input in dBFS, output normalized 0..1 (bigger = louder vs baseline)
stepBand :: Float -> BandState -> (Float, BandState)
stepBand x (BandState s l p) =
  let -- EMA params
      αs = 0.25 -- short EMA (fast)
      αL_up = 0.01 -- long EMA rises *slowly*
      αL_down = 0.08 -- long EMA falls faster (prevents “fade-out”)
      peakHold = 0.985 -- decay per step

      -- update short EMA
      s' = s + αs * (x - s)

      -- asymmetric long EMA
      αL = if x > l then αL_up else αL_down
      l' = l + αL * (x - l)

      -- optional peak-hold (unused in norm below, but handy for UI)
      p' = max x (p * peakHold)

      -- normalize:
      -- use dB *difference* with small dead-zone, then scale to 0..1
      dead = 1.5 -- dB that we ignore
      delta = max 0 (s' - l' - dead) -- dB above baseline
      scale = 8.0 -- ~how many dB map to full scale
      norm = clamp01 (delta / scale)
   in (norm, BandState s' l' p')

clamp01 :: Float -> Float
clamp01 = max 0 . min 1

processAudio :: (HortureEffects m hdl l) => Maybe Scene -> Horture m l hdl (Maybe Scene)
processAudio Nothing = return Nothing
processAudio (Just s) = do
  mapM_ playAudio $ s ^. audio
  return $ Just s

countFrame :: Horture m l hdl ()
countFrame = do
  fc <- asks (^. frameCounter)
  liftIO . atomically . modifyTVar' fc $ (+ 1)

clearView :: Horture m l hdl ()
clearView = liftIO $ GL.clear [ColorBuffer, DepthBuffer]

updateView :: Horture m l hdl ()
updateView = asks _glWin >>= liftIO . GLFW.swapBuffers

pollEvents :: (HortureEffects m hdl l) => Scene -> Float -> Float -> Horture m l hdl (Maybe Scene)
pollEvents s timeNow dt = do
  pollGLFWEvents
  pollWindowEnvironment
  pollHortureEvents timeNow dt s

pollGLFWEvents :: Horture m l hdl ()
pollGLFWEvents = liftIO GLFW.pollEvents

deltaTime :: Float -> Horture m l hdl (Float, Float)
deltaTime startTime =
  getTime >>= \currentTime -> return $ (currentTime - startTime, currentTime)

setTime :: Float -> Horture m l hdl ()
setTime = liftIO . GLFW.setTime . realToFrac

getTime :: Horture m l hdl Float
getTime =
  liftIO GLFW.getTime >>= \case
    Nothing -> throwError . HE $ "GLFW not running or initialized"
    Just t -> return $ realToFrac t

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

  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 1)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)

  win <-
    GLFW.createWindow 1024 1024 hortureName Nothing Nothing >>= \case
      Nothing -> throwError . userError $ "Failed to create GLFW window"
      Just win -> return win
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowCloseCallback win (Just shutdown)

  GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden
  GLFW.setStickyKeysInputMode win GLFW.StickyKeysInputMode'Disabled
  GLFW.setStickyMouseButtonsInputMode win GLFW.StickyMouseButtonsInputMode'Disabled

  return win

resizeWindow' :: GLFW.WindowSizeCallback
resizeWindow' _ w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

shutdown :: GLFW.Window -> IO ()
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  void exitSuccess

initResources ::
  (GLsizei, GLsizei) ->
  [(FilePath, Asset)] ->
  Maybe FilePath ->
  IO (HortureScreenProgram, HortureDynamicImageProgram, HortureBackgroundProgram, HortureFontProgram)
initResources (w, h) images mFont = do
  -- Initialize OpenGL primitives.
  initBaseQuad
  -- Set color stuff.
  GL.clearColor $= Color4 0.1 0.1 0.1 1
  effs <- initShaderEffects
  hsp <- initHortureScreenProgram (w, h) effs
  dip <- initHortureDynamicImageProgram images
  ftp <- initHortureFontProgram mFont
  hbp <- initHortureBackgroundProgram
  -- Generic OpenGL configuration.
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  return (hsp, dip, hbp, ftp)

initShaderEffects :: IO (Map.Map ShaderEffect [HortureShaderProgram])
initShaderEffects = do
  loadShaderBS "passthrough.shader" VertexShader passthroughVertexShader >>= compileAndLinkShaderEffects
  where
    compileAndLinkShaderEffects vsp = do
      let shaderProgs =
            [ (Barrel, [barrelShader]),
              (Stitch, [stitchShader]),
              (Glitch, [aiIsWonderful]),
              (Blur, [blurVShader, blurHShader]),
              (Flashbang, [flashbangShader]),
              (Kaleidoscope, [kaleidoscopeShader]),
              (Cycle, [cycleColoursShader]),
              (Blink, [blinkShader]),
              (Mirror, [mirrorShader]),
              (Invert, [invertShader]),
              (Toonify, [toonShader]),
              (Audiophile, [audioShader]),
              (BassRealityWarp, [bassRealityWarp])
            ]
          buildLinkAndUniform p = do
            hsp <- loadShaderBS "shadereffect.shader" FragmentShader p >>= linkShaderProgram . (: [vsp])
            lifetimeUniform <- uniformLocation hsp "lifetime"
            dtUniform <- uniformLocation hsp "dt"
            timeSinceUniform <- uniformLocation hsp "timeSinceStart"
            dominatingFreqUniform <- uniformLocation hsp "frequencies"
            randomUniform <- uniformLocation hsp "rng"
            return
              HortureShaderProgram
                { _hortureShaderProgramShader = hsp,
                  _hortureShaderProgramLifetimeUniform = lifetimeUniform,
                  _hortureShaderProgramTimeSinceUniform = timeSinceUniform,
                  _hortureShaderProgramDtUniform = dtUniform,
                  _hortureShaderProgramFrequenciesUniform = dominatingFreqUniform,
                  _hortureShaderProgramRandomUniform = randomUniform
                }
      Map.fromList <$> mapM (sequenceRight . second (sequence . (buildLinkAndUniform <$>))) shaderProgs
    sequenceRight :: (ShaderEffect, IO [HortureShaderProgram]) -> IO (ShaderEffect, [HortureShaderProgram])
    sequenceRight (st, action) = (st,) <$> action

initHortureScreenProgram :: (GLsizei, GLsizei) -> Map.Map ShaderEffect [HortureShaderProgram] -> IO HortureScreenProgram
initHortureScreenProgram (w, h) effs = do
  -- Shader program.
  vsp <- loadShaderBS "mvp.shader" VertexShader mvpVertexShader
  fsp <- loadShaderBS "display.shader" FragmentShader displayShader
  prog <- linkShaderProgram [vsp, fsp]
  currentProgram $= Just prog

  screenTexUniform <- uniformLocation prog "imgTexture"
  uniform screenTexUniform $= Index1 (0 :: GLint)

  -- Initialize source texture holding captured window image.
  backTexture <- genObjectName
  let !anyPixelData = PixelData BGRA UnsignedByte nullPtr
  textureBinding Texture2D $= Just backTexture
  texImage2D
    Texture2D
    NoProxy
    0
    RGBA'
    (TextureSize2D w h)
    0
    anyPixelData
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

  renderedTexture <- genObjectName
  textureBinding Texture2D $= Just renderedTexture
  texImage2D
    Texture2D
    NoProxy
    0
    RGBA'
    (TextureSize2D w h)
    0
    anyPixelData
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

  -- FRAMEBUFFER SETUP BEGIN
  -- fb is the framebuffer, grouping our textures.
  fb <- genObjectName
  bindFramebuffer Framebuffer $= fb

  -- Configure framebuffer. We will directly bind our source texture as a
  -- colorattachment.
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D renderedTexture 0
  drawBuffers $= [FBOColorAttachment 0]
  -- FRAMEBUFFER SETUP END

  -- Setup uniforms.
  let proj = curry projectionForAspectRatio (fromIntegral w) (fromIntegral h)
  projectionUniform <- uniformLocation prog "proj"
  m44ToGLmatrix proj >>= (uniform projectionUniform $=)

  let view = Cam3D.camMatrix (Cam3D.fpsCamera @Float)
  viewUniform <- uniformLocation prog "view"
  m44ToGLmatrix view >>= (uniform viewUniform $=)

  let model = curry scaleForAspectRatio (fromIntegral w) (fromIntegral h)
  modelUniform <- uniformLocation prog "model"
  m44ToGLmatrix model >>= (uniform modelUniform $=)

  timeUniform <- uniformLocation prog "dt"
  uniform timeUniform $= (0 :: Float)

  uvInsetUniform <- uniformLocation prog "uvInset"
  uniform uvInsetUniform $= (Vertex2 1 1 :: Vertex2 GLfloat)

  bindFramebuffer Framebuffer $= defaultFramebufferObject

  return $
    HortureScreenProgram
      { _hortureScreenProgramShader = prog,
        _hortureScreenProgramShaderEffects = effs,
        _hortureScreenProgramModelUniform = modelUniform,
        _hortureScreenProgramProjectionUniform = projectionUniform,
        _hortureScreenProgramTextureUniform = screenTexUniform,
        _hortureScreenProgramUVInsetUniform = uvInsetUniform,
        _hortureScreenProgramViewUniform = viewUniform,
        _hortureScreenProgramTimeUniform = timeUniform,
        _hortureScreenProgramFramebuffer = fb,
        _hortureScreenProgramTextureObject = renderedTexture,
        _hortureScreenProgramBackTextureObject = backTexture,
        _hortureScreenProgramTextureUnit = screenTextureUnit
      }
  where
    screenTextureUnit = TextureUnit 0

-- | initBaseQuad sets up a basic quad which will be used by all objects in
-- horture for drawing textures.
initBaseQuad :: IO ()
initBaseQuad = do
  vao <- genObjectName
  vbo <- genObjectName
  veo <- genObjectName
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

initHortureFontProgram :: Maybe FilePath -> IO HortureFontProgram
initHortureFontProgram mFont = do
  vpg <- loadShaderBS "fontvertex.shader" VertexShader gifVertexShader
  fpg <- loadShaderBS "fontfragment.shader" FragmentShader fontFragmentShader
  fontProg <- linkShaderProgram [vpg, fpg]
  modelUniform <- uniformLocation fontProg "model"
  fontTexUniform <- uniformLocation fontProg "fontTexture"
  opacityUniform <- uniformLocation fontProg "opacity"
  currentProgram $= Just fontProg
  glyphs <- case mFont of
    Just font -> runFontLoader fontTextureUnit fontTexUniform (loadFont font)
    Nothing -> return Map.empty
  m44ToGLmatrix identityM44 >>= (uniform modelUniform $=)
  return
    HortureFontProgram
      { _hortureFontProgramShader = fontProg,
        _hortureFontProgramTextureUnit = fontTextureUnit,
        _hortureFontProgramTexUniform = fontTexUniform,
        _hortureFontProgramModelUniform = modelUniform,
        _hortureFontProgramOpacityUniform = opacityUniform,
        _hortureFontProgramChars = glyphs
      }
  where
    fontTextureUnit = TextureUnit 3

initHortureDynamicImageProgram :: [(FilePath, Asset)] -> IO HortureDynamicImageProgram
initHortureDynamicImageProgram images = do
  vspg <- loadShaderBS "gifvertex.shader" VertexShader gifVertexShader
  fspg <- loadShaderBS "giffragment.shader" FragmentShader gifFragmentShader
  gifProg <- linkShaderProgram [vspg, fspg]
  gifModelUniform <- uniformLocation gifProg "model"
  gifTexUni <- uniformLocation gifProg "gifTexture"
  gifTexIndex <- uniformLocation gifProg "index"
  currentProgram $= Just gifProg
  m44ToGLmatrix identityM44 >>= (uniform gifModelUniform $=)

  vspg <- loadShaderBS "imagevertex.shader" VertexShader gifVertexShader
  fspg <- loadShaderBS "imagefragment.shader" FragmentShader imageFragmentShader
  imgProg <- linkShaderProgram [vspg, fspg]
  imgModelUniform <- uniformLocation imgProg "model"
  imgTexUnit <- uniformLocation imgProg "imgTexture"
  currentProgram $= Just imgProg
  m44ToGLmatrix identityM44 >>= (uniform imgModelUniform $=)
  (loaderResult, loaderState) <-
    runTextureLoader
      ( LC
          { _loaderConfigPreloadedImages = images,
            _loaderConfigGifProg = gifProg,
            _loaderConfigGifTexUniform = gifTexUni,
            _loaderConfigGifTextureUnit = gifTextureUnit,
            _loaderConfigImageProg = imgProg,
            _loaderConfigImageTexUniform = imgTexUnit,
            _loaderConfigImageTextureUnit = imgTextureUnit
          }
      )
      def
      loadGifsGL
  hortureAssets <- case loaderResult of
    Left _ -> exitFailure
    _otherwise -> do
      return $ loaderState ^. resolvedGifs
  return $
    HortureDynamicImageProgram
      { _hortureDynamicImageProgramGifProgram =
          HortureGifProgram
            { _hortureGifProgramShader = gifProg,
              _hortureGifProgramModelUniform = gifModelUniform,
              _hortureGifProgramIndexUniform = gifTexIndex,
              _hortureGifProgramTextureUnit = gifTextureUnit
            },
        _hortureDynamicImageProgramImageProgram =
          HortureImageProgram
            { _hortureImageProgramShader = imgProg,
              _hortureImageProgramTextureUnit = imgTextureUnit,
              _hortureImageProgramModelUniform = imgModelUniform
            },
        _hortureDynamicImageProgramAssets = hortureAssets
      }
  where
    gifTextureUnit = TextureUnit 4
    imgTextureUnit = TextureUnit 5

initHortureBackgroundProgram :: IO HortureBackgroundProgram
initHortureBackgroundProgram = do
  bvsp <- loadShaderBS "passthrough.shader" VertexShader passthroughVertexShader
  bfsp <- loadShaderBS "background.shader" FragmentShader backgroundShader
  backgroundProg <- linkShaderProgram [bvsp, bfsp]
  backgroundTimeUniform <- uniformLocation backgroundProg "time"
  return $
    HortureBackgroundProgram
      { _hortureBackgroundProgramShader = backgroundProg,
        _hortureBackgroundProgramTimeUniform = backgroundTimeUniform,
        _hortureBackgroundProgramTextureUnit = backgroundTextureUnit
      }
  where
    backgroundTextureUnit = TextureUnit 1

data SizeUpdate = GLFWUpdate !(Int, Int) | XUpdate !(CInt, CInt) deriving (Show, Eq)
