module Horture.Loader.TextureLoader
  ( TextureLoader,
    runLoader,
    isGif,
    loadGifsGL,
    defaultGifDelay,
    defaultGifDirectory,
    loadDirectory,
  )
where

import Codec.Picture
import Codec.Picture.Gif
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (readFile)
import Data.Functor ((<&>))
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.Vector.Storable (toList)
import Data.Word
import Foreign.Marshal.Array (withArray)
import Graphics.Rendering.OpenGL hiding (get, imageHeight)
import Horture.Gif
import Horture.Loader.Config
import Horture.Loader.Error
import Horture.Loader.State
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath (takeBaseName, takeExtension)
import Prelude hiding (readFile)

-- TODO: Use the output from the preloader to load textures instead of doing it
-- here all over again.
type TextureLoader a = ExceptT LoaderError (ReaderT LoaderConfig (StateT LoaderState IO)) a

-- | Loader is responsible for loading external Horture resources, like GIFs, and
-- prepare them to be used afterwards.
runLoader :: LoaderConfig -> LoaderState -> Loader a -> IO (Either LoaderError a, LoaderState)
runLoader lc ls = flip runStateT ls . flip runReaderT lc . runExceptT

defaultGifDirectory :: FilePath
defaultGifDirectory = "."

defaultGifDelay :: GifDelay
defaultGifDelay = 200

type Loader a = ExceptT LoaderError (ReaderT LoaderConfig (StateT LoaderState IO)) a

loadGifsGL :: Loader ()
loadGifsGL = do
  gifFiles <- asks _lcgifDirectory >>= liftIO . makeAbsolute >>= liftIO . loadDirectory <&> filter isGif
  loadProgram
  glGifs <- mapM loadGifGL gifFiles
  storeGifs glGifs
  unloadProgram

storeGifs :: [HortureGIF] -> Loader ()
storeGifs gifs =
  modify
    ( \s ->
        s
          { _resolvedGifs =
              mergeMapsUpdateWithRight
                (_resolvedGifs s)
                (Map.fromList . map (\g -> (_gifFullPath g, g)) $ gifs)
          }
    )
  where
    mergeMapsUpdateWithRight :: Map.Map FilePath c -> Map.Map FilePath c -> Map.Map FilePath c
    mergeMapsUpdateWithRight =
      Map.merge
        Map.preserveMissing'
        Map.preserveMissing'
        (Map.zipWithMatched $ \_fp _old new -> new)

loadProgram :: Loader ()
loadProgram = asks _lcgifProg >>= (currentProgram $=) . Just

unloadProgram :: Loader ()
unloadProgram = currentProgram $= Nothing

readImagesAndMetadata :: FilePath -> Loader ([Image PixelRGBA8], [GifDelay], (Int, Int))
readImagesAndMetadata gif = do
  content <- liftIO $ readFile gif
  imgs@(i : _) <- case decodeGifImages content of
    Left err -> throwError $ "decoding gif: " <> err
    Right [] -> throwError "decoded gif without images"
    Right imgs@(ImageRGBA8 _ : _) -> mapM stripDynamicImage imgs
    Right _ -> throwError "only supporting ImageRGBA8 gif types"
  delaysms <- case getDelaysGifImages content of
    Left err -> throwError $ "retrieving gif delays: " <> err
    Right ds -> return ds
  return (imgs, delaysms, (imageWidth i, imageHeight i))

stripDynamicImage :: DynamicImage -> Loader (Image PixelRGBA8)
stripDynamicImage (ImageRGBA8 i) = return i
stripDynamicImage _ = throwError "heterogenous gif array encountered"

loadGifGL :: FilePath -> Loader HortureGIF
loadGifGL gif = do
  (imgs, delaysms, (w, h)) <- readImagesAndMetadata gif
  rawGifData <- case foldImageData imgs of
    Left err -> throwError err
    Right imgdata -> return imgdata

  nu <- asks _lcGifTextureUnit
  activeTexture $= nu
  gifTexObject <- genObjectName @TextureObject
  textureBinding Texture2DArray $= Just gifTexObject

  liftIO $
    withArray rawGifData $ \ptr -> do
      let pixelData = PixelData RGBA UnsignedByte ptr
      texImage3D
        Texture2DArray
        NoProxy
        0
        RGBA8
        (TextureSize3D (fromIntegral w) (fromIntegral h) (fromIntegral . length $ imgs))
        0
        pixelData
      generateMipmap' Texture2DArray
  gifTexUni <- asks _lcgifTexUniform
  uniform gifTexUni $= nu

  return $ HortureGIF gif (takeBaseName gif) gifTexObject (length imgs) delaysms

foldImageData :: [Image PixelRGBA8] -> Either LoaderError [Word8]
foldImageData [] = Left "no image data available to fold"
foldImageData imgs = Right $ concatMap (toList . imageData) imgs

isGif :: FilePath -> Bool
isGif fp = takeExtension fp == ".gif"

loadDirectory :: FilePath -> IO [FilePath]
loadDirectory fp = listDirectory fp <&> map ((fp ++ "/") ++)
