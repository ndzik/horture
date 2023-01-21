{-# LANGUAGE FlexibleInstances #-}

-- | This module defines some behaviours which can be used to modify how an
-- object is displayed during its lifetime.
module Horture.Behaviour
  ( pulse,
    convolute,
    circle,
    shake,
    moveTo,
    rotate,
    audiophile,
  )
where

import Control.Lens
import Graphics.GLUtil.Camera3D (deg2rad)
import Horture.Object
import Linear.Matrix
import Linear.Quaternion hiding (rotate)
import Linear.V3
import Linear.V4
import Linear.Vector

-- | pulse applies a pulsing effect, similar to a heartbeat.
pulse :: Float -> Float -> Float -> Behaviour
pulse min amplifier frequency = Behaviour BehaviourPulse $ \_ t o ->
  o & scale
    %~ ( !*!
           V4
             (V4 (min + amplifier * sin (frequency * realToFrac t)) 0 0 0)
             (V4 0 (min + amplifier * sin (frequency * realToFrac t)) 0 0)
             (V4 0 0 (min + amplifier * sin (frequency * realToFrac t)) 0)
             (V4 0 0 0 1)
       )

-- | convolute applies a convolution effect where an object appears to be
-- wrapping up into itself.
convolute :: Behaviour
convolute = Behaviour BehaviourConvolute $ \_ t o ->
  o & scale
    %~ ( !+!
           V4
             (V4 (0.5 * (sin . realToFrac $ t)) 0 0 0)
             (V4 0 (0.5 * (cos . realToFrac $ t)) 0 0)
             (V4 0 0 (0.5 * (sin . realToFrac $ t)) 0)
             (V4 0 0 0 1)
       )

shake :: Float -> Float -> Float -> Behaviour
shake amp scale frequency = Behaviour BehaviourShake $ \_ t o ->
  o & pos
    %~ ( ^+^
           V3
             ((amp / 100) * sin (scale * frequency * realToFrac t))
             ((amp / 100) * (- cos (scale * frequency * realToFrac t)))
             0
       )

circle :: Float -> Float -> Behaviour
circle scale frequency = Behaviour BehaviourCircle $ \_ t o ->
  o & pos
    %~ ( ^+^
           V3
             (scale * sin (frequency * realToFrac t))
             (scale * cos (frequency * realToFrac t))
             0
       )

moveTo :: V3 Float -> Behaviour
moveTo target = Behaviour BehaviourMoveTo $ \_ t o ->
  o & pos %~ lerp (realToFrac t) target

rotate :: Float -> Behaviour
rotate factor = Behaviour BehaviourRotate $ \_ _ o ->
  o & orientation %~ \oq -> axisAngle (V3 0 0 (-1)) (deg2rad factor) * oq

audiophile :: Behaviour
audiophile = Behaviour BehaviourAudiophile $ \(bass, _, _) _ o ->
  let amplifier = 0.0001
   in o & scale
        %~ ( !*!
               V4
                 (V4 (1 - amplifier * realToFrac bass) 0 0 0)
                 (V4 0 (1 - amplifier * realToFrac bass) 0 0)
                 (V4 0 0 (1 - amplifier * realToFrac bass) 0)
                 (V4 0 0 0 1)
           )
