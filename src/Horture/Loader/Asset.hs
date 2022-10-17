module Horture.Loader.Asset
  ( Asset (..),
  )
where

import Codec.Picture
import Data.Word
import Foreign.ForeignPtr

-- | Asset identifies an asset usable by horture. An asset contains all
-- necessary data for further construction and processing in a pure manner.
data Asset = AssetGif
  { _assetGifWidth :: !Int,
    _assetGifHeight :: !Int,
    _assetGifDelays :: ![GifDelay],
    _assetGifImages :: !(ForeignPtr Word8)
  }
