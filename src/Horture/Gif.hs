module Horture.Gif (HortureGIF (..)) where

import Codec.Picture.Gif
import Graphics.Rendering.OpenGL

-- TODO: Do I want to track the ShaderProgram here too? Would be redundant
-- state.
data HortureGIF = HortureGIF
  { _gifFullPath :: !FilePath,
    _gifName :: !String,
    _gifTextureUnit :: !TextureUnit,
    _gifTextureObject :: !TextureObject,
    _gifNumOfImgs :: !Int,
    _gifDelay :: !GifDelay
  }
  deriving (Show)
