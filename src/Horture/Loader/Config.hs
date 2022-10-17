{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Horture.Loader.Config where

import Codec.Picture.Gif
import Control.Lens.TH
import Graphics.Rendering.OpenGL

data LoaderConfig = LC
  { -- | Directory where gif files are presumably stored.
    _lcgifDirectory :: !FilePath,
    -- | Shaderprogram to load when binding textures.
    _lcgifProg :: !Program,
    -- | Location for sampler2DArray in fragment shader.
    _lcgifTexUniform :: !UniformLocation,
    -- | Default delay to use for gifs without a specified delay.
    _lcdefaultGifDelay :: !GifDelay,
    -- | Texture unit which holds GIF images.
    _lcGifTextureUnit :: !TextureUnit
  }
  deriving (Show)

-- | PreloaderConfig is the configuration for the asset preloader.
newtype PreloaderConfig = PLC
  { -- | All directories of interest containing relevant files.
    _preloaderConfigGifDirectory :: FilePath
  }
  deriving (Show)

makeFields ''PreloaderConfig
