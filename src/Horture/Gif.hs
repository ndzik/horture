module Horture.Gif (HortureGIF (..)) where

import Codec.Picture.Gif
import Graphics.Rendering.OpenGL

data HortureGIF = HortureGIF
  { _gifFullPath :: !FilePath,
    _gifName :: !String,
    _gifTextureUnit :: !TextureUnit,
    _gifTextureObject :: !TextureObject,
    _gifDelay :: !GifDelay
  }
  deriving (Show)
