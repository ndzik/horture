module Horture.Object where

import Control.Lens.TH
import Data.Default
import Horture.Audio
import Linear.Matrix
import Linear.Quaternion
import Linear.V3
import Linear.V4

-- | Lifetime describes for how long an object can live. Either forever or for
-- a limited time. Limited defines the lifetime in seconds.
data Lifetime = Forever | Limited !Float deriving (Show, Eq)

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
    _birth :: !Float,
    -- | How will this object behave during its lifetime. An empty list means a
    -- static object. Each behaviour has an associated lifetime and time of
    -- birth.
    _behaviours :: ![(Behaviour, Float, Lifetime)]
  }
  deriving (Show)

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
        _birth = 0,
        _behaviours = []
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

isStillAlive :: Float -> Lifetime -> Float -> Bool
isStillAlive _ Forever _ = True
isStillAlive timeNow (Limited s) tob = (timeNow - tob) <= s

data BehaviourType
  = BehaviourAudiophile
  | BehaviourShake
  | BehaviourRotate
  | BehaviourMoveTo
  | BehaviourCircle
  | BehaviourPulse
  | BehaviourBob
  | BehaviourBounce
  | BehaviourSway
  | BehaviourWobble
  | BehaviourSpiralTo
  | BehaviourOrbit
  | BehaviourBreathe
  | BehaviourJitter
  | BehaviourEaseTo
  | BehaviourFlipX
  deriving (Show, Eq, Enum, Bounded)

-- | Each object can have multiple behaviours attached to it. The behaviour
-- decides how this object will be displayed during its lifetime depending on
-- how long it already exists.
data Behaviour = Behaviour !BehaviourType !BehaviourApply

type BehaviourApply = FFTSnapshot -> Float -> Object -> BehaviourDelta

data BehaviourDelta = BehaviourDelta
  { bdPos :: V3 Float,
    bdScale :: V3 Float,
    bdRot :: Quaternion Float
  }

instance Semigroup BehaviourDelta where
  BehaviourDelta p1 s1 r1 <> BehaviourDelta p2 s2 r2 = BehaviourDelta (p1 + p2) (s1 * s2) (r2 * r1)

instance Monoid BehaviourDelta where mempty = BehaviourDelta 0 (V3 1 1 1) (axisAngle (V3 0 0 1) 0)

instance Eq Behaviour where
  (Behaviour bt1 _) == (Behaviour bt2 _) = bt1 == bt2

instance Show Behaviour where
  show (Behaviour bt _) = show bt

makeLenses ''Object
