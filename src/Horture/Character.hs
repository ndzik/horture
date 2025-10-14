module Horture.Character where

import Control.Lens
import Graphics.Rendering.OpenGL
import Horture.Object
import Linear.V2

data Character = Character
  { _characterTextureID :: !TextureObject,
    _characterSize :: !(V2 Int),
    _characterBearing :: !(V2 Int),
    _characterAdvance :: !Int,
    _characterLetter :: !Char
  }
  deriving (Show)

-- | A WordObject is a renderable unit of possibly multiple Characters.
data WordObject = WordObject
  { _wordObjectObject :: !Object,
    _wordObjectLetters :: ![Character]
  }
  deriving (Show)

characterHeight :: Int
characterHeight = 128

baseScale :: Float
baseScale = 1 / 3

makeFields ''Character
makeFields ''WordObject
