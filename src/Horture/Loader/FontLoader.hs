module Horture.Loader.FontLoader where

import Control.Lens
import Control.Loop
import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Data.Word
import Foreign hiding (void)
-- import FreeType
import Graphics.GL.Functions (glPixelStorei)
import Graphics.GL.Tokens
import Graphics.Rendering.OpenGL
import Horture.Character
import Linear.V2

data FontLoaderEnv = FontLoaderEnv
  { _fontLoaderEnvTextureUnit :: !TextureUnit,
    _fontLoaderEnvTexUni :: !UniformLocation
  }

makeLenses ''FontLoaderEnv

type FontLoader a = ReaderT FontLoaderEnv IO a

runFontLoader :: TextureUnit -> UniformLocation -> FontLoader a -> IO a
runFontLoader tu tuni act = runReaderT act (FontLoaderEnv tu tuni)

loadFont :: FilePath -> FontLoader (Map.Map Char Character)
loadFont fp = do
  undefined

--   tu <- asks _fontLoaderEnvTextureUnit
--   fontTexUni <- asks _fontLoaderEnvTexUni
--   activeTexture $= tu
--   liftIO $
--     ft_With_FreeType $ \lib ->
--       ft_With_Face lib fp 0 $ \face -> do
--         -- Set the pixel font size we'd like to extract. 0 width let's the
--         -- width automatically be calculated based on the given height.
--         ft_Set_Pixel_Sizes face 0 (fromIntegral characterHeight)
--         -- Disable OpenGL alignment requirement.
--         glPixelStorei GL_UNPACK_ALIGNMENT 2
--         -- Load the first 128 ASCII characters into textures.
--         let loadCharacter char = do
--               ft_Load_Char face char FT_LOAD_RENDER
--               to <- genObjectName @TextureObject
--               textureBinding Texture2D $= Just to
--               glyphSlot <- peek . frGlyph =<< peek face
--               let glyphBitmap = gsrBitmap glyphSlot
--                   chWidth = bWidth glyphBitmap
--                   chHeight = bRows glyphBitmap
--                   chLeft = gsrBitmap_left glyphSlot
--                   chTop = gsrBitmap_top glyphSlot
--                   chAdvance = vX . gsrAdvance $ glyphSlot
--               withOutlineTexture (chWidth, chHeight) (bBuffer glyphBitmap) $ \buf -> do
--                 let pixelData = PixelData RG UnsignedByte buf
--                 texImage2D
--                   Texture2D
--                   NoProxy
--                   0
--                   RG8
--                   (TextureSize2D (fromIntegral chWidth) (fromIntegral chHeight))
--                   0
--                   pixelData
--                 textureWrapMode Texture2D S $= (Repeated, ClampToBorder)
--                 textureWrapMode Texture2D T $= (Repeated, ClampToBorder)
--                 textureFilter Texture2D $= ((Linear', Nothing), Linear')
--                 uniform fontTexUni $= tu
--                 return
--                   ( toEnum . fromIntegral $ char,
--                     Character
--                       { _characterTextureID = to,
--                         _characterSize = fromIntegral <$> V2 chWidth chHeight,
--                         _characterBearing = fromIntegral <$> V2 chLeft chTop,
--                         _characterAdvance = fromIntegral chAdvance,
--                         _characterLetter = toEnum . fromIntegral $ char
--                       }
--                   )
--         Map.fromList <$> mapM loadCharacter [32 .. 127]

withOutlineTexture :: (Word32, Word32) -> Ptr Word8 -> (Ptr Word8 -> IO b) -> IO b
withOutlineTexture dim src action = do
  buf <- outlineTexture dim src
  res <- action buf
  free buf
  return res

outlineTexture :: (Word32, Word32) -> Ptr Word8 -> IO (Ptr Word8)
outlineTexture (w, h) buf = do
  mallocBytes (fromIntegral $ w * h * 2) >>= \newBuf -> do
    numLoop 0 (fromIntegral w - 1) $ \x -> do
      numLoop 0 (fromIntegral h - 1) $ \y -> do
        let i = x + y * fromIntegral w
        alpha <- peekByteOff @Word8 buf i
        case alpha of
          0x00 -> do
            ns <- mapM (\(nx, ny) -> peekByteOff @Word8 buf (nx + ny * fromIntegral w)) $ neighbors (x, y) (fromIntegral w, fromIntegral h)
            if any (/= 0x00) ns
              then do
                pokeByteOff @Word8 newBuf (i * 2) 0x00
                pokeByteOff @Word8 newBuf (i * 2 + 1) 0xFF
              else do
                pokeByteOff @Word8 newBuf (i * 2) 0x00
                pokeByteOff @Word8 newBuf (i * 2 + 1) 0x00
          a -> do
            pokeByteOff @Word8 newBuf (i * 2) a
            pokeByteOff @Word8 newBuf (i * 2 + 1) 0xFF
    return newBuf
  where
    neighbors (x, y) (maxX, maxY) = [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1], (x' /= x || y' /= y) && x' >= 0 && y' >= 0 && x' < maxX && y' < maxY]
