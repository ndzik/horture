module Horture.Program where

import Control.Lens.TH
import Data.Map.Strict
import Graphics.Rendering.OpenGL
import qualified Data.Map.Strict as Map
import Horture.Effect
import Horture.Character
import Horture.Asset

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
    _hortureScreenProgramShaderEffects :: !(Map ShaderEffect [HortureShaderProgram]),
    -- | All shaders receive the same uniforms.
    _hortureScreenProgramModelUniform :: !UniformLocation,
    _hortureScreenProgramProjectionUniform :: !UniformLocation,
    _hortureScreenProgramViewUniform :: !UniformLocation,
    _hortureScreenProgramTimeUniform :: !UniformLocation,
    _hortureScreenProgramTextureObject :: !TextureObject,
    _hortureScreenProgramBackTextureObject :: !TextureObject,
    _hortureScreenProgramFramebuffer :: !FramebufferObject,
    _hortureScreenProgramTextureUnit :: !TextureUnit
  }
  deriving (Show)

data HortureDynamicImageProgram = HortureDynamicImageProgram
  { _hortureDynamicImageProgramGifProgram :: !HortureGifProgram,
    _hortureDynamicImageProgramImageProgram :: !HortureImageProgram,
    _hortureDynamicImageProgramAssets :: !(Map FilePath HortureAsset)
  }
  deriving (Show)

data HortureGifProgram = HortureGifProgram
  { _hortureGifProgramShader :: !Program,
    _hortureGifProgramModelUniform :: !UniformLocation,
    _hortureGifProgramIndexUniform :: !UniformLocation,
    _hortureGifProgramTextureUnit :: !TextureUnit
  }
  deriving (Show)

data HortureImageProgram = HortureImageProgram
  { _hortureImageProgramShader :: !Program,
    _hortureImageProgramTextureUnit :: !TextureUnit,
    _hortureImageProgramModelUniform :: !UniformLocation
  }
  deriving (Show)

data HortureFontProgram = HortureFontProgram
  { _hortureFontProgramShader :: !Program
  , _hortureFontProgramTextureUnit :: !TextureUnit
  , _hortureFontProgramTexUniform :: !UniformLocation
  , _hortureFontProgramModelUniform :: !UniformLocation
  , _hortureFontProgramOpacityUniform :: !UniformLocation
  , _hortureFontProgramChars :: !(Map.Map Char Character)
  } deriving (Show)

data HortureBackgroundProgram = HortureBackgroundProgram
  { _hortureBackgroundProgramShader :: !Program,
    _hortureBackgroundProgramTimeUniform :: !UniformLocation,
    _hortureBackgroundProgramTextureUnit :: !TextureUnit
  }
  deriving (Show)

-- | A shader in horture which can be used as an effect.
data HortureShaderProgram = HortureShaderProgram
  { _hortureShaderProgramShader :: !Program,
    _hortureShaderProgramLifetimeUniform :: !UniformLocation,
    _hortureShaderProgramDtUniform :: !UniformLocation,
    _hortureShaderProgramFrequenciesUniform :: !UniformLocation,
    _hortureShaderProgramRandomUniform :: !UniformLocation
  }
  deriving (Show)

makeFields ''HortureScreenProgram
makeFields ''HortureDynamicImageProgram
makeFields ''HortureFontProgram
makeFields ''HortureImageProgram
makeFields ''HortureGifProgram
makeFields ''HortureShaderProgram
makeFields ''HortureBackgroundProgram
