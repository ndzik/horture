module Horture.Loader.FilePreloader
  ( FilePreloader,
    loadGifsInMemory,
    runPreloader,
  )
where

import Codec.Picture
import Codec.Picture.Gif
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString (readFile)
import Data.Word
import Foreign (Storable (poke, sizeOf), plusPtr)
import Foreign.ForeignPtr
import Horture.Loader.Asset
import Horture.Loader.Config
import Horture.Loader.Error
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath (takeExtension)
import Prelude hiding (readFile)

type FilePreloader a = ExceptT LoaderError (ReaderT PreloaderConfig IO) a

runPreloader :: PreloaderConfig -> FilePreloader a -> IO (Either LoaderError a)
runPreloader lc = flip runReaderT lc . runExceptT

loadGifsInMemory :: FilePreloader [(FilePath, Asset)]
loadGifsInMemory = do
  gifFiles <- asks (^. gifDirectory) >>= liftIO . makeAbsolute >>= liftIO . loadDirectory <&> filter isGif
  mapM readImagesAndMetadata gifFiles

isGif :: FilePath -> Bool
isGif fp = takeExtension fp == ".gif"

loadDirectory :: FilePath -> IO [FilePath]
loadDirectory fp = listDirectory fp <&> map ((fp ++ "/") ++)

readImagesAndMetadata :: FilePath -> FilePreloader (FilePath, Asset)
readImagesAndMetadata gif = do
  content <- liftIO $ readFile gif
  (i, imgs) <- case decodeGifImages content of
    Left err -> throwError $ "decoding gif: " <> err
    Right imgs@(ImageRGBA8 i : _) -> unifyDynamicImages (imageWidth i, imageHeight i) imgs >>= \imgVector -> return (i, imgVector)
    Right _ -> throwError "invalid gif encountered"
  delaysms <- case getDelaysGifImages content of
    Left err -> throwError $ "retrieving gif delays: " <> err
    Right ds -> return ds
  return
    ( gif,
      AssetGif
        { _assetGifWidth = imageWidth i,
          _assetGifHeight = imageHeight i,
          _assetGifDelays = delaysms,
          _assetGifImages = imgs
        }
    )

unifyDynamicImages :: (Int, Int) -> [DynamicImage] -> FilePreloader (ForeignPtr Word8)
unifyDynamicImages (w, h) imgs = do
  let stride = w * h * 4 * szByte
      szByte = sizeOf @Word8 undefined
  farr <- liftIO $ mallocForeignPtrArray (stride * length imgs)
  liftIO $
    withForeignPtr farr $ \arr -> do
      forM_ (zip [0 ..] imgs) $ \(idx, img) -> case img of
        ImageRGBA8 i -> do
          _ <-
            imageIPixels
              ( \(x, y, PixelRGBA8 r g b a) -> do
                  poke (plusPtr arr (idx * stride + x + y * w + 0 * szByte)) r
                  poke (plusPtr arr (idx * stride + x + y * w + 1 * szByte)) g
                  poke (plusPtr arr (idx * stride + x + y * w + 2 * szByte)) b
                  poke (plusPtr arr (idx * stride + x + y * w + 3 * szByte)) a
                  return (PixelRGBA8 r g b a)
              )
              i
          return ()
        ImageRGB8 i -> do
          _ <-
            imageIPixels
              ( \(x, y, PixelRGB8 r g b) -> do
                  poke (plusPtr arr (idx * stride + x + y * w + 0 * szByte)) r
                  poke (plusPtr arr (idx * stride + x + y * w + 1 * szByte)) g
                  poke (plusPtr arr (idx * stride + x + y * w + 2 * szByte)) b
                  poke (plusPtr @_ @Word8 arr (idx * stride + x + y * w + 3 * szByte)) 0xff
                  return (PixelRGB8 r g b)
              )
              i
          return ()
        _otherwise -> return ()
  return farr
