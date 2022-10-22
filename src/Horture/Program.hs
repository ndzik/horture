{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Horture.Program where

import Control.Lens.TH
import Data.Map.Strict
import Graphics.Rendering.OpenGL
import Horture.Effect
import Horture.Gif

-- | HortureScreenProgram contains all OpenGL informations to handle the
-- rendering and displaying of the captured screen image.
data HortureScreenProgram = HortureScreenProgram
  { -- | Final screen program shader, which is responsible for finally
    -- rendering the effect transformed captured image to the screen.
    _hortureScreenProgramShader :: !Program,
    -- | Preconfigured shader effects, which receive the captured screen as a
    -- texture input. This texture could have been modified by by a previous
    -- shader effect, s.t. ShaderEffects effectively compose. A shadereffect
    -- can be comprised of multiple shaderprograms.
    -- Shaderprograms are applied in reversed list order for efficiency
    -- reasons.
    _hortureScreenProgramShaderEffects :: !(Map ShaderEffect [Program]),
    -- | All shaders receive the same uniforms.
    _hortureScreenProgramModelUniform :: !UniformLocation,
    _hortureScreenProgramProjectionUniform :: !UniformLocation,
    _hortureScreenProgramViewUniform :: !UniformLocation,
    _hortureScreenProgramTimeUniform :: !UniformLocation,
    _hortureScreenProgramTextureObject :: !TextureObject,
    _hortureScreenProgramFramebuffer :: !FramebufferObject,
    _hortureScreenProgramTextureUnit :: !TextureUnit
  }
  deriving (Show)

data HortureGifProgram = HortureGifProgram
  { _hortureGifProgramShader :: !Program,
    _hortureGifProgramModelUniform :: !UniformLocation,
    _hortureGifProgramIndexUniform :: !UniformLocation,
    _hortureGifProgramAssets :: !(Map FilePath HortureGif),
    _hortureGifProgramTextureUnit :: !TextureUnit
  }
  deriving (Show)

makeFields ''HortureScreenProgram
makeFields ''HortureGifProgram
