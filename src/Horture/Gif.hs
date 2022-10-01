module Horture.Gif (HortureGIF (..)) where

import Codec.Picture.Gif
import Graphics.Rendering.OpenGL

data HortureGIF = HortureGIF
  { _gifFullPath :: !FilePath,
    _gifName :: !String,
    _gifTextureObject :: !TextureObject,
    _gifNumOfImgs :: !Int,
    _gifDelays :: ![GifDelay]
  }
  deriving (Show)
