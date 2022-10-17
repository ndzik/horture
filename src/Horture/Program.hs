{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Horture.Program where

import Control.Lens.TH
import Data.Map
import Graphics.Rendering.OpenGL
import Horture.Gif

data HortureScreenProgram = HortureScreenProgram
  { _hortureScreenProgramShader :: !Program,
    _hortureScreenProgramModelUniform :: !UniformLocation,
    _hortureScreenProgramProjectionUniform :: !UniformLocation,
    _hortureScreenProgramViewUniform :: !UniformLocation,
    _hortureScreenProgramTimeUniform :: !UniformLocation,
    _hortureScreenProgramTextureObject :: !TextureObject,
    _hortureScreenProgramTextureUnit :: !TextureUnit
  }
  deriving (Show)

data HortureGifProgram = HortureGifProgram
  { _hortureGifProgramShader :: !Program,
    _hortureGifProgramModelUniform :: !UniformLocation,
    _hortureGifProgramIndexUniform :: !UniformLocation,
    _hortureGifProgramAssets :: !(Map FilePath HortureGIF),
    _hortureGifProgramTextureUnit :: !TextureUnit
  }
  deriving (Show)

makeFields ''HortureScreenProgram
makeFields ''HortureGifProgram
