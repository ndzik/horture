{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Horture.Asset where

import Codec.Picture.Gif
import Control.Lens.TH
import Graphics.Rendering.OpenGL

data HortureAsset
  = HortureGif
      { _hortureAssetFullPath :: !FilePath,
        _hortureAssetName :: !String,
        _hortureAssetTextureObject :: !TextureObject,
        _hortureAssetNumOfImgs :: !Int,
        _hortureAssetDelays :: ![GifDelay]
      }
  | HortureImage
      { _hortureAssetFullPath :: !FilePath,
        _hortureAssetName :: !String,
        _hortureAssetTextureObject :: !TextureObject
      }
  deriving (Show)

makeFields ''HortureAsset
