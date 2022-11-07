module Horture.Loader.FilePreloader
  ( FilePreloader,
    loadGifsInMemory,
    runPreloader,
    loadDirectory,
  )
where

import Codec.Picture
import Codec.Picture.Gif
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString (readFile)
import Data.Word
import Foreign (Storable (poke), plusPtr)
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
  (nrOfImgs, (w, h), imgs) <- case decodeGifImages content of
    Left err -> throwError $ "decoding gif: " <> err
    Right imgs@(img : _) -> do
      ((w, h), imgVector) <- case img of
        ImageRGBA8 i -> do
          let w = imageWidth i
              h = imageHeight i
          imgVector <- unifyDynamicImages (w, h) imgs
          return ((w, h), imgVector)
        ImageRGB8 i -> do
          let w = imageWidth i
              h = imageHeight i
          imgVector <- unifyDynamicImages (w, h) imgs
          return ((w, h), imgVector)
        _otherwise -> throwError "wrong encoding encountered"
      return (length imgs, (w, h), imgVector)
    Right _ -> throwError "invalid gif encountered"
  delaysms <- case getDelaysGifImages content of
    Left err -> throwError $ "retrieving gif delays: " <> err
    Right ds -> return ds
  return
    ( gif,
      AssetGif
        { _assetGifWidth = w,
          _assetGifHeight = h,
          _assetNumberOfFrames = nrOfImgs,
          _assetGifDelays = delaysms,
          _assetGifImages = imgs
        }
    )

-- | unifyDynamicImages tightly packs the list of DynamicImages into RGBA8
-- format.
-- This is horribly slow and would benefit from a custom C decoder which
-- tightly packs GIFs when decoding.
unifyDynamicImages :: (Int, Int) -> [DynamicImage] -> FilePreloader (ForeignPtr Word8)
unifyDynamicImages (w, h) imgs = do
  let -- stride is the number of bytes each image occupies.
      stride = w * h * 4
  -- bytes in a single horizontal scanline.
  farr <- liftIO $ mallocForeignPtrArray (stride * length imgs)
  liftIO $
    withForeignPtr farr $ \arr -> do
      forM_ (zip [0 ..] imgs) $ \(idx, img) -> case img of
        ImageRGBA8 i -> do
          _ <-
            imageIPixels
              ( \(x, y, PixelRGBA8 r g b a) -> do
                  poke (plusPtr arr (idx * stride + (x * 4) + (y * 4 * w) + 0)) r
                  poke (plusPtr arr (idx * stride + (x * 4) + (y * 4 * w) + 1)) g
                  poke (plusPtr arr (idx * stride + (x * 4) + (y * 4 * w) + 2)) b
                  poke (plusPtr arr (idx * stride + (x * 4) + (y * 4 * w) + 3)) a
                  return (PixelRGBA8 r g b a)
              )
              i
          return ()
        ImageRGB8 i -> do
          _ <-
            imageIPixels
              ( \(x, y, PixelRGB8 r g b) -> do
                  poke (plusPtr arr (idx * stride + (x * 4) + (y * 4 * w) + 0)) r
                  poke (plusPtr arr (idx * stride + (x * 4) + (y * 4 * w) + 1)) g
                  poke (plusPtr arr (idx * stride + (x * 4) + (y * 4 * w) + 2)) b
                  poke (plusPtr @_ @Word8 arr (idx * stride + (x * 4) + (y * 4 * w) + 3)) 0xff
                  return (PixelRGB8 r g b)
              )
              i
          return ()
        _otherwise -> return ()
  return farr
