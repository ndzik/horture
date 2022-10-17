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
import Horture.Gif
import Horture.Loader.Asset
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
loadGifsGL = do
  loadProgram
  loadGifTextures >>= storeGifs
  unloadProgram

storeGifs :: [HortureGif] -> TextureLoader ()
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

loadProgram :: TextureLoader ()
loadProgram = asks (^. gifProg) >>= (currentProgram $=) . Just

unloadProgram :: TextureLoader ()
unloadProgram = currentProgram $= Nothing

loadGifTextures :: TextureLoader [HortureGif]
loadGifTextures = do
  nu <- asks (^. gifTextureUnit)
  activeTexture $= nu
  asks (^. preloadedGifs)
    >>= mapM
      ( \(fp, AssetGif w h n ds dptr) -> do
          gifTexObject <- genObjectName @TextureObject
          textureBinding Texture2DArray $= Just gifTexObject

          liftIO $
            withForeignPtr dptr $ \ptr -> do
              let pixelData = PixelData RGBA UnsignedByte ptr
              texImage3D
                Texture2DArray
                NoProxy
                0
                RGBA8
                (TextureSize3D (fromIntegral w) (fromIntegral h) (fromIntegral n))
                0
                pixelData
              generateMipmap' Texture2DArray
          gifTexUni <- asks (^. gifTexUniform)
          uniform gifTexUni $= nu
          return $ HortureGif fp (takeBaseName fp) gifTexObject n ds
      )
