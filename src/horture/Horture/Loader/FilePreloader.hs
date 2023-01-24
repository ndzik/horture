{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Horture.Loader.FilePreloader
  ( FilePreloader,
    loadAssetsInMemory,
    runPreloader,
    loadDirectory,
  )
where

import Codec.Picture hiding (readImage)
import Codec.Picture.Gif
import Control.Lens
import Control.Loop
import Control.Monad.Except
import Data.Foldable (foldrM)
import Control.Monad.Reader
import Data.ByteString (readFile)
import Data.Vector.Storable (Vector, unsafeWith)
import Data.Word
import Foreign hiding (void)
import Horture.Loader.Asset
import Horture.Loader.Config
import Horture.Loader.Error
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath (takeExtension)
import Prelude hiding (readFile)

type FilePreloader a = ExceptT LoaderError (ReaderT PreloaderConfig IO) a

runPreloader :: PreloaderConfig -> FilePreloader a -> IO (Either LoaderError a)
runPreloader lc = flip runReaderT lc . runExceptT

loadAssetsInMemory :: FilePreloader [(FilePath, Asset)]
loadAssetsInMemory = do
  dirContent <- asks (^. assetDirectory) >>= liftIO . makeAbsolute >>= liftIO . loadDirectory
  let loadAssetsIgnoreInvalidExt asset loaded = ((: loaded) <$> readAssets asset) `catchError` \case
        LoaderUnsupportedAssetType _ -> return loaded
        err -> throwError err
  foldrM loadAssetsIgnoreInvalidExt [] dirContent

loadDirectory :: FilePath -> IO [FilePath]
loadDirectory fp = listDirectory fp <&> map ((fp ++ "/") ++)

readAssets :: FilePath -> FilePreloader (FilePath, Asset)
readAssets fp = case takeExtension fp of
  ".gif" -> readImagesAndMetadata fp
  ".png" -> readPngImage fp
  _else -> throwError . LoaderUnsupportedAssetType $ fp

readPngImage :: FilePath -> FilePreloader (FilePath, Asset)
readPngImage png = do
  img <-
    liftIO (readFile png) <&> decodePng >>= \case
      Left err -> throwError . LoaderDecodingPNG $ show err
      Right r -> return r
  asset <- decodeDynamicImage img
  return (png, asset)

readImage ::
  forall colorDataType a.
  (Storable colorDataType, Storable a) =>
  ImageType ->
  Int ->
  Int ->
  Vector colorDataType ->
  FilePreloader (ForeignPtr a)
readImage it w h sourceData = do
  let bytesPerImage = w * h * numPixelComponents * bytesPerColor
      bytesPerColor = sizeOf @colorDataType undefined
      numPixelComponents = case it of
        RGB8 -> 3
        RGBA8 -> 4
        RGB16 -> 3
        RGBA16 -> 4
  sinkData <- liftIO $ mallocForeignPtrArray bytesPerImage
  readImageOffset it w h sourceData sinkData 0

readImageOffset ::
  forall colorDataType a.
  (Storable colorDataType, Storable a) =>
  ImageType ->
  Int ->
  Int ->
  Vector colorDataType ->
  ForeignPtr a ->
  Int ->
  FilePreloader (ForeignPtr a)
readImageOffset it w h sourceData sinkData offset = do
  let bytesPerColor = sizeOf @colorDataType undefined
      numPixelComponents = case it of
        RGB8 -> 3
        RGBA8 -> 4
        RGB16 -> 3
        RGBA16 -> 4
  liftIO $
    withForeignPtr sinkData $ \arr -> do
      numLoop 0 h $ \y -> do
        numLoop 0 w $ \x -> do
          void . liftIO . unsafeWith sourceData $ \imgptr -> do
            let r = 0
                g = 1
                b = 2
                a = 3
            readColorComponent (x, y) w bytesPerColor numPixelComponents imgptr r
              >>= writeColorComponent (x, y) w bytesPerColor numPixelComponents arr r offset
            readColorComponent (x, y) w bytesPerColor numPixelComponents imgptr g
              >>= writeColorComponent (x, y) w bytesPerColor numPixelComponents arr g offset
            readColorComponent (x, y) w bytesPerColor numPixelComponents imgptr b
              >>= writeColorComponent (x, y) w bytesPerColor numPixelComponents arr b offset
            -- Could be better doing it statically, but we will see.
            when (it == RGBA8 || it == RGBA16) $ do
              readColorComponent (x, y) w bytesPerColor numPixelComponents imgptr a
                >>= writeColorComponent (x, y) w bytesPerColor numPixelComponents arr a offset
  return sinkData
  where
    readColorComponent :: (Int, Int) -> Int -> Int -> Int -> Ptr colorDataType -> Int -> IO colorDataType
    readColorComponent (x, y) w bytesPerColor numPixelComponents imgptr component =
      peekByteOff @colorDataType imgptr $ x * numPixelComponents * bytesPerColor + y * numPixelComponents * bytesPerColor * w + component * bytesPerColor
    writeColorComponent :: (Int, Int) -> Int -> Int -> Int -> Ptr a -> Int -> Int -> colorDataType -> IO ()
    writeColorComponent (x, y) w bytesPerColor numPixelComponents arr component offset value =
      poke (plusPtr arr (offset + (x * numPixelComponents * bytesPerColor) + (y * numPixelComponents * bytesPerColor * w) + component * bytesPerColor)) value

decodeDynamicImage :: DynamicImage -> FilePreloader Asset
decodeDynamicImage img = do
  case img of
    ImageRGB8 i ->
      let w = imageWidth i
          h = imageHeight i
       in AssetImage w h RGB8 <$> readImage RGB8 w h (imageData i)
    ImageRGBA8 i ->
      let w = imageWidth i
          h = imageHeight i
       in AssetImage w h RGBA8 <$> readImage RGBA8 w h (imageData i)
    ImageRGB16 i ->
      let w = imageWidth i
          h = imageHeight i
       in AssetImage w h RGB16 <$> readImage RGB16 w h (imageData i)
    ImageRGBA16 i ->
      let w = imageWidth i
          h = imageHeight i
       in AssetImage w h RGBA16 <$> readImage RGBA16 w h (imageData i)
    _otherwise -> throwError $ LoaderUnsupportedAssetType ""

readImagesAndMetadata :: FilePath -> FilePreloader (FilePath, Asset)
readImagesAndMetadata gif = do
  content <- liftIO $ readFile gif
  (nrOfImgs, (w, h), imgs, imgType) <- case decodeGifImages content of
    Left err -> throwError . LoaderDecodingGif $ "decoding gif: " <> err
    Right imgs@(img : _) -> do
      let l = length imgs
      case img of
        ImageRGBA8 i -> do
          let w = imageWidth i
              h = imageHeight i
          imgdata <- unifyDynamicImages (w, h) imgs
          return (l, (w, h), imgdata, RGBA8)
        ImageRGB8 i -> do
          let w = imageWidth i
              h = imageHeight i
          imgdata <- unifyDynamicImages (w, h) imgs
          return (l, (w, h), imgdata, RGB8)
        _otherwise -> throwError . LoaderDecodingGif $ "wrong encoding encountered"
    Right _ -> throwError . LoaderDecodingGif $ "invalid gif encountered"
  delaysms <- case getDelaysGifImages content of
    Left err -> throwError . LoaderDecodingGif $ "retrieving gif delays: " <> err
    Right ds -> return ds
  return
    ( gif,
      AssetGif
        { _assetGifWidth = w,
          _assetGifHeight = h,
          _assetGifType = imgType,
          _assetNumberOfFrames = nrOfImgs,
          _assetGifDelays = delaysms,
          _assetGifImages = imgs
        }
    )

-- | unifyDynamicImages tightly packs the list of DynamicImages into a format
-- which can be loaded to the GPU and is compatible with OpenGL.
-- This is horribly slow and would benefit from a custom C decoder which
-- tightly packs GIFs when decoding.
unifyDynamicImages :: (Int, Int) -> [DynamicImage] -> FilePreloader (ForeignPtr Word8)
unifyDynamicImages _ [] = throwError LoaderEmptyGif
unifyDynamicImages (w, h) imgs@(img : _) = do
  -- stride is the number of bytes each image occupies.
  stride <- case img of
    ImageRGBA8 _ -> return $ w * h * 4
    ImageRGB8 _ -> return $ w * h * 3
    _otherwise -> throwError $ LoaderDecodingGif "Gif with unsupported image type"
  -- bytes in a single horizontal scanline.
  farr <- liftIO $ mallocForeignPtrArray (stride * length imgs)
  forM_ (zip [0 ..] imgs) $ \(idx, img) -> case img of
    ImageRGBA8 i -> void $ readImageOffset RGBA8 w h (imageData i) farr (idx * stride)
    ImageRGB8 i -> void $ readImageOffset RGB8 w h (imageData i) farr (idx * stride)
    _otherwise -> return ()
  return farr
