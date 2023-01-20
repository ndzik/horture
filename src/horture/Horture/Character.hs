module Horture.Character where

import Control.Lens
import Linear.V2
import Graphics.Rendering.OpenGL

data Character = Character { _characterTextureID :: !TextureObject
                           , _characterSize :: !(V2 Int)
                           , _characterBearing :: !(V2 Int)
                           , _characterAdvance :: !Int
                           } deriving (Show)

makeLenses ''Character
