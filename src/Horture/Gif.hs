{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Horture.Gif where

import Codec.Picture.Gif
import Control.Lens.TH
import Graphics.Rendering.OpenGL

data HortureGif = HortureGif
  { _hortureGifFullPath :: !FilePath,
    _hortureGifName :: !String,
    _hortureGifTextureObject :: !TextureObject,
    _hortureGifNumOfImgs :: !Int,
    _hortureGifDelays :: ![GifDelay]
  }
  deriving (Show)

makeFields ''HortureGif
