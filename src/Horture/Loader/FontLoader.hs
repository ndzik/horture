{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Horture.Loader.FontLoader
  ( FontLoader,
    FontLoaderEnv (..),
    runFontLoader,
    loadFont,
  )
where

import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Data.Word
import Foreign hiding (void)
import Foreign.C
import Graphics.GL.Functions (glPixelStorei)
import Graphics.GL.Tokens
import Graphics.Rendering.OpenGL
import Horture.Character
import Linear.V2

data FontLoaderEnv = FontLoaderEnv
  { _fontLoaderEnvTextureUnit :: !TextureUnit,
    _fontLoaderEnvTexUni :: !UniformLocation
  }

type FontLoader a = ReaderT FontLoaderEnv IO a

runFontLoader :: TextureUnit -> UniformLocation -> FontLoader a -> IO a
runFontLoader tu tuni act = runReaderT act (FontLoaderEnv tu tuni)

loadFont :: FilePath -> FontLoader (Map.Map Char Character)
loadFont fp = loadFontInternal fp characterHeight

data CFTLContext

data CFTLFace

data CGlyph = CGlyph
  { gPixels :: Ptr Word8,
    gWidth :: CInt,
    gHeight :: CInt,
    gBearingX :: CInt,
    gBearingY :: CInt,
    gAdvance :: CInt,
    gCode :: Word32
  }

instance Storable CGlyph where
  sizeOf _ = (8) + 6 * 4 + 4 -- pointer + 6 ints + u32 (platforms differ; safer to 'peek' field-by-field)
  alignment _ = alignment (nullPtr :: Ptr ())
  peek p = do
    pix <- peekByteOff p 0
    w <- peekByteOff p (ptrSize)
    h <- peekByteOff p (ptrSize + 4)
    bx <- peekByteOff p (ptrSize + 8)
    by <- peekByteOff p (ptrSize + 12)
    adv <- peekByteOff p (ptrSize + 16)
    cp <- peekByteOff p (ptrSize + 20)
    pure $ CGlyph pix w h bx by adv cp
    where
      ptrSize = sizeOf (nullPtr :: Ptr ())
  poke _ _ = error "not needed"

foreign import ccall unsafe "ftl_create" c_ftl_create :: IO (Ptr CFTLContext)

foreign import ccall unsafe "ftl_destroy" c_ftl_destroy :: Ptr CFTLContext -> IO ()

foreign import ccall unsafe "ftl_face_open" c_ftl_face_open :: Ptr CFTLContext -> CString -> CUInt -> IO (Ptr CFTLFace)

foreign import ccall unsafe "ftl_face_close" c_ftl_face_close :: Ptr CFTLFace -> IO ()

foreign import ccall unsafe "ftl_render_glyph" c_ftl_render_glyph :: Ptr CFTLFace -> Word32 -> CInt -> Ptr CGlyph -> IO CInt

foreign import ccall unsafe "ftl_free_glyph" c_ftl_free_glyph :: Ptr CGlyph -> IO ()

loadFontInternal :: FilePath -> Int -> FontLoader (Map.Map Char Character)
loadFontInternal path pixelHeight = do
  tu <- asks _fontLoaderEnvTextureUnit
  fontTexUni <- asks _fontLoaderEnvTexUni
  activeTexture $= tu
  ctx <- liftIO c_ftl_create

  let uploadGlyphRG8WithOutline :: CGlyph -> IO TextureObject
      uploadGlyphRG8WithOutline CGlyph {..} = do
        to <- genObjectName
        textureBinding Texture2D $= Just to
        glPixelStorei GL_UNPACK_ALIGNMENT 1
        glPixelStorei GL_UNPACK_ROW_LENGTH 0

        let w = fromIntegral gWidth
            h = fromIntegral gHeight

        if w > 0 && h > 0
          then
            texImage2D
              Texture2D
              NoProxy
              0
              RG8
              (TextureSize2D w h)
              0
              (PixelData RG UnsignedByte gPixels)
          else
            texImage2D
              Texture2D
              NoProxy
              0
              RG8
              (TextureSize2D 1 1)
              0
              (PixelData RG UnsignedByte nullPtr)

        textureWrapMode Texture2D S $= (Repeated, ClampToBorder)
        textureWrapMode Texture2D T $= (Repeated, ClampToBorder)
        textureFilter Texture2D $= ((Linear', Nothing), Linear')
        uniform fontTexUni $= tu

        pure to

  liftIO $ withCString path $ \cpath -> do
    face <- c_ftl_face_open ctx cpath (fromIntegral pixelHeight)
    m <- go uploadGlyphRG8WithOutline face 32 127 Map.empty
    c_ftl_face_close face
    c_ftl_destroy ctx
    pure m
  where
    outlinePixel = 16
    go _upload _face i maxI acc | i > maxI = pure acc
    go upload face i maxI acc =
      allocaBytes glyphSize $ \pg -> do
        rc <- c_ftl_render_glyph face (fromIntegral (fromEnum (toEnum i :: Char))) outlinePixel pg
        if rc == 0
          then go upload face (i + 1) maxI acc
          else do
            g <- peekGlyph pg
            tex <- upload g
            let ch = toEnum i
                w = fromIntegral (gWidth g)
                h = fromIntegral (gHeight g)
                bx = fromIntegral (gBearingX g)
                by = fromIntegral (gBearingY g)
                adv = fromIntegral (gAdvance g)
                charRec =
                  Character
                    { _characterTextureID = tex,
                      _characterSize = V2 w h,
                      _characterBearing = V2 bx by,
                      _characterAdvance = adv,
                      _characterLetter = ch
                    }
            c_ftl_free_glyph pg
            go upload face (i + 1) maxI (Map.insert ch charRec acc)

    glyphSize = 8 + 6 * 4 + 4
    peekGlyph :: Ptr CGlyph -> IO CGlyph
    peekGlyph = peek
