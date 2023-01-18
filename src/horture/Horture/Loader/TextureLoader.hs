module Horture.Loader.TextureLoader
  ( TextureLoader,
    runTextureLoader,
    loadGifsGL,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Foreign.ForeignPtr (withForeignPtr)
import Graphics.Rendering.OpenGL hiding (get, imageHeight)
import Horture.Asset
import Horture.Loader.Asset (Asset (..))
import qualified Horture.Loader.Asset as Asset
import Horture.Loader.Config
import Horture.Loader.Error
import Horture.Loader.State
import System.FilePath (takeBaseName)
import Prelude hiding (readFile)

type TextureLoader a = ExceptT LoaderError (ReaderT LoaderConfig (StateT LoaderState IO)) a

-- | Loader is responsible for loading external Horture resources, like GIFs, and
-- prepare them to be used afterwards.
runTextureLoader :: LoaderConfig -> LoaderState -> TextureLoader a -> IO (Either LoaderError a, LoaderState)
runTextureLoader lc ls = flip runStateT ls . flip runReaderT lc . runExceptT

loadGifsGL :: TextureLoader ()
loadGifsGL = loadGifTextures >>= storeGifs

storeGifs :: [HortureAsset] -> TextureLoader ()
storeGifs gifs =
  resolvedGifs %= mergeMapsUpdateWithLeft (Map.fromList . map (\g -> (g ^. fullPath, g)) $ gifs)
  where
    mergeMapsUpdateWithLeft :: Map.Map FilePath c -> Map.Map FilePath c -> Map.Map FilePath c
    mergeMapsUpdateWithLeft l r =
      Map.merge
        Map.preserveMissing'
        Map.preserveMissing'
        (Map.zipWithMatched $ \_fp _old new -> new)
        r
        l

withProgram :: Program -> TextureLoader a -> TextureLoader a
withProgram prog action = do
  loadProgram prog
  res <- action
  unloadProgram
  return res

loadProgram :: Program -> TextureLoader ()
loadProgram = (currentProgram $=) . Just

unloadProgram :: TextureLoader ()
unloadProgram = currentProgram $= Nothing

loadGifTextures :: TextureLoader [HortureAsset]
loadGifTextures = do
  nu <- asks (^. gifTextureUnit)
  iu <- asks (^. imageTextureUnit)
  imgProg <- asks (^. imageProg)
  gifProg <- asks (^. gifProg)
  asks (^. preloadedAssets)
    >>= mapM
      ( \(fp, asset) -> case asset of
          AssetGif w h n imgType ds dptr -> withProgram gifProg $ do
            activeTexture $= nu
            gifTexObject <- genObjectName @TextureObject
            textureBinding Texture2DArray $= Just gifTexObject

            let (oglInternalType, oglPixelType, oglDataType) = resolveImgType imgType
            liftIO $
              withForeignPtr dptr $ \ptr -> do
                let pixelData = PixelData oglPixelType oglDataType ptr
                texImage3D
                  Texture2DArray
                  NoProxy
                  0
                  oglInternalType
                  (TextureSize3D (fromIntegral w) (fromIntegral h) (fromIntegral n))
                  0
                  pixelData
                generateMipmap' Texture2DArray
            gifTexUni <- asks (^. gifTexUniform)
            uniform gifTexUni $= nu
            return $ HortureGif fp (takeBaseName fp) gifTexObject n ds
          AssetImage w h imgType dptr -> withProgram imgProg $ do
            activeTexture $= iu
            imageTexObject <- genObjectName @TextureObject
            textureBinding Texture2D $= Just imageTexObject

            let (oglInternalType, oglPixelType, oglDataType) = resolveImgType imgType
            liftIO $
              withForeignPtr dptr $ \ptr -> do
                let pixelData = PixelData oglPixelType oglDataType ptr
                texImage2D
                  Texture2D
                  NoProxy
                  0
                  oglInternalType
                  (TextureSize2D (fromIntegral w) (fromIntegral h))
                  0
                  pixelData
                generateMipmap' Texture2D
            imageTexUni <- asks (^. imageTexUniform)
            uniform imageTexUni $= iu
            return $ HortureImage fp (takeBaseName fp) imageTexObject
      )

resolveImgType :: Asset.ImageType -> (PixelInternalFormat, PixelFormat, DataType)
resolveImgType Asset.RGB8 = (RGB8, RGB, UnsignedByte)
resolveImgType Asset.RGBA8 = (RGBA8, RGBA, UnsignedByte)
resolveImgType Asset.RGB16 = (RGB16, RGB, TwoBytes)
resolveImgType Asset.RGBA16 = (RGBA16, RGBA, TwoBytes)
