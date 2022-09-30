{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Horture.Object
  ( Object (..),
    Lifetime (..),
    Renderable (..),
    isStillAlive,
  )
where

import Data.Default
import Linear.Matrix
import Linear.Quaternion
import Linear.V3
import Linear.V4

-- | Lifetime describes for how long an object can live. Either forever or for
-- a limited time. Limited defines the lifetime in seconds.
data Lifetime = Forever | Limited !Double deriving (Show, Eq)

-- | Object is a horture object which forms the basis for all objects in a
-- horture scene, which can have effects applied to them and rendered.
data Object = Object
  { -- | Current position of this object.
    _pos :: !(V3 Float),
    -- | Current orientation of this object.
    _orientation :: !(Quaternion Float),
    -- | Current scale of this object.
    _scale :: !(M44 Float),
    -- | How long this object is supposed to live.
    _lifetime :: !Lifetime,
    -- | When was this object created.
    _birth :: !Double
  }
  deriving (Show, Eq)

instance Default Object where
  def =
    Object
      { _pos = V3 0 0 (-1),
        _orientation = Quaternion 1 (V3 0 0 0),
        _scale =
          V4
            (V4 1 0 0 0)
            (V4 0 1 0 0)
            (V4 0 0 1 0)
            (V4 0 0 0 1),
        _lifetime = Forever,
        _birth = 0
      }

-- | Renderable is every object capable of giving a model description in form of
-- a M44 matrix.
class Renderable o where
  model :: o -> M44 Float

instance Renderable Object where
  model o = m
    where
      trans = mkTransformation @Float (_orientation o) (_pos o)
      m = trans !*! _scale o

isStillAlive :: Double -> Object -> Bool
isStillAlive timeNow o = case _lifetime o of
  Forever -> True
  Limited s -> (timeNow - _birth o) <= s
