{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Horture.Object
  ( Object (..),
    Lifetime (..),
    Renderable (..),
    defScreen,
    defGif,
    isStillAlive,
  )
where

import Linear.Matrix
import Linear.Quaternion
import Linear.V3
import Linear.V4

-- Lifetime describes for how long an object can live. Either forever or for a
-- limited time. Limited defines the lifetime in seconds.
data Lifetime = Forever | Limited Double deriving (Show, Eq)

data TextureType
  = Background
  | Gif
  | Image
  deriving (Show, Eq)

-- Voice will be eventual soundfiles associated with gifs.
data Voice = Voice
  deriving (Show, Eq)

-- Object is a horture object which can be rendered. Every horture object is in
-- essence a quad with a position, orientation and scale. For animation
-- purposes each object also has a time of birth and overall lifetime.
data Object = Object
  { _id :: Int,
    _pos :: V3 Float,
    _orientation :: Quaternion Float,
    _scale :: M44 Float,
    _lifetime :: Lifetime,
    _birth :: Double,
    _textureType :: TextureType,
    _textureLength :: Int,
    _delay :: Int,
    _voice :: Maybe Voice
    -- _attributes :: [Attribute]
  }
  deriving (Show, Eq)

-- Renderable is every object capable of giving a model description in form of
-- a M44 matrix.
class Renderable o where
  model :: o -> M44 Float

instance Renderable Object where
  model o = m
    where
      trans = mkTransformation @Float (_orientation o) (V3 0 0 0)
      m = trans !*! _scale o

defScreen :: Object
defScreen =
  Object
    { _id = 0,
      _pos = V3 0 0 0,
      _orientation = Quaternion 1 (V3 0 0 0),
      _scale =
        V4
          (V4 1 0 0 0)
          (V4 0 1 0 0)
          (V4 0 0 1 0)
          (V4 0 0 0 1),
      _lifetime = Forever,
      _birth = 0,
      _textureType = Background,
      _textureLength = 1,
      _delay = 0,
      _voice = Nothing
    }

defGif :: Int -> Double -> Lifetime -> Int -> Int -> Object
defGif id birth lifetime numOfImgs delayms =
  Object
    { _id = id,
      _pos = V3 0 0 0,
      _orientation = Quaternion 1 (V3 0 0 0),
      _scale =
        V4
          (V4 1 0 0 0)
          (V4 0 1 0 0)
          (V4 0 0 1 0)
          (V4 0 0 0 1),
      _lifetime = lifetime,
      _birth = birth,
      _textureType = Gif,
      _textureLength = numOfImgs,
      _delay = delayms,
      _voice = Nothing
    }

isStillAlive :: Double -> Object -> Bool
isStillAlive timeNow o = case _lifetime o of
  Forever -> True
  Limited s -> (timeNow - _birth o) <= s
