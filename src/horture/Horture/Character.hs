module Horture.Character where

import Control.Lens
import Linear.V2
import Graphics.Rendering.OpenGL
import Horture.Object

data Character = Character { _characterTextureID :: !TextureObject
                           , _characterSize :: !(V2 Int)
                           , _characterBearing :: !(V2 Int)
                           , _characterAdvance :: !Int
                           , _characterLetter :: !Char
                           } deriving (Show)

-- | A WordObject is a renderable unit of possibly multiple Characters.
data WordObject = WordObject { _wordObjectObject :: !Object
                             , _wordObjectLetters :: ![Character]
                             } deriving (Show)

characterHeight :: Int
characterHeight = 64

baseScale :: Float
baseScale = 1/3

makeFields ''Character
makeFields ''WordObject
