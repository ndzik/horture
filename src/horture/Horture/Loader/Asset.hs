module Horture.Loader.Asset
  ( Asset (..),
    ImageType (..),
  )
where

import Codec.Picture
import Data.Word
import Foreign.ForeignPtr

-- | Asset identifies an asset usable by horture. An asset contains all
-- necessary data for further construction and processing in a pure manner.
data Asset
  = AssetGif
      { _assetGifWidth :: !Int,
        _assetGifHeight :: !Int,
        _assetNumberOfFrames :: !Int,
        _assetGifType :: !ImageType,
        _assetGifDelays :: ![GifDelay],
        _assetGifImages :: !(ForeignPtr Word8)
      }
  | AssetImage
      { _assetImageWidth :: !Int,
        _assetImageHeight :: !Int,
        _assetImageType :: !ImageType,
        _assetImageData :: !(ForeignPtr Word8)
      }

data ImageType
  = RGB8
  | RGB16
  | RGBA8
  | RGBA16
  deriving (Show, Eq)
