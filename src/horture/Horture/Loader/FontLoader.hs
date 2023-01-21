module Horture.Loader.FontLoader where

import Control.Monad.Reader
import FreeType
import Graphics.Rendering.OpenGL
import Graphics.GL.Functions (glPixelStorei)
import Graphics.GL.Tokens
import Control.Lens
import Linear.V2
import Foreign.Storable
import qualified Data.Map.Strict as Map
import Horture.Character

data FontLoaderEnv = FontLoaderEnv { _fontLoaderEnvTextureUnit :: !TextureUnit
                                   , _fontLoaderEnvTexUni :: !UniformLocation
                                   }

makeLenses ''FontLoaderEnv

type FontLoader a = ReaderT FontLoaderEnv IO a

runFontLoader :: TextureUnit -> UniformLocation -> FontLoader a -> IO a
runFontLoader tu tuni act = runReaderT act (FontLoaderEnv tu tuni)

loadFont :: FilePath -> FontLoader (Map.Map Char Character)
loadFont fp = do
  tu <- asks _fontLoaderEnvTextureUnit
  fontTexUni <- asks _fontLoaderEnvTexUni
  activeTexture $= tu
  liftIO $ ft_With_FreeType $ \lib ->
      ft_With_Face lib fp 0 $ \face -> do
        -- Set the pixel font size we'd like to extract. 0 width let's the
        -- width automatically be calculated based on the given height.
        ft_Set_Pixel_Sizes face 0 48
        -- Disable OpenGL alignment requirement.
        glPixelStorei GL_UNPACK_ALIGNMENT 1
        -- Load the first 128 ASCII characters into textures.
        let loadCharacter char = do
              ft_Load_Char face char FT_LOAD_RENDER
              to <- genObjectName @TextureObject
              textureBinding Texture2D $= Just to
              glyphSlot <- peek . frGlyph =<< peek face
              let glyphBitmap = gsrBitmap glyphSlot
                  pixelData = PixelData Red UnsignedByte (bBuffer glyphBitmap)
                  chWidth = bWidth glyphBitmap
                  chHeight = bRows glyphBitmap
                  chLeft = gsrBitmap_left glyphSlot
                  chTop = gsrBitmap_top glyphSlot
                  chAdvance = vX . gsrAdvance $ glyphSlot
              texImage2D
                Texture2D
                NoProxy
                0
                R8
                (TextureSize2D (fromIntegral chWidth) (fromIntegral chHeight))
                0
                pixelData
              textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
              textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
              textureFilter Texture2D $= ((Linear', Nothing), Linear')
              liftIO $ generateMipmap' Texture2D
              uniform fontTexUni $= tu
              return (toEnum . fromIntegral $ char, Character {
                                      _characterTextureID = to
                                      , _characterSize = fromIntegral <$> V2 chWidth chHeight
                                      , _characterBearing = fromIntegral <$> V2 chLeft chTop
                                      , _characterAdvance = fromIntegral chAdvance
                                      })
        Map.fromList <$> mapM loadCharacter [0..128]
