{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Horture.Loader.Config where

import Control.Lens.TH
import Graphics.Rendering.OpenGL
import Horture.Loader.Asset

data LoaderConfig = LC
  { -- | Directory where gif files are presumably stored.
    _loaderConfigPreloadedGifs :: ![(FilePath, Asset)],
    -- | Shaderprogram to load when binding textures.
    _loaderConfigGifProg :: !Program,
    -- | Location for sampler2DArray in fragment shader.
    _loaderConfigGifTexUniform :: !UniformLocation,
    -- | Texture unit which holds GIF images.
    _loaderConfigGifTextureUnit :: !TextureUnit
  }

-- | PreloaderConfig is the configuration for the asset preloader.
newtype PreloaderConfig = PLC
  { -- | All directories of interest containing relevant files.
    _preloaderConfigGifDirectory :: FilePath
  }
  deriving (Show)

makeFields ''PreloaderConfig
makeFields ''LoaderConfig
