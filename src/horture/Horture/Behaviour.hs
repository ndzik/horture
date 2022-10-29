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
pulse min amplifier frequency t o =
  o & scale
    %~ ( !+!
           V4
             (V4 (min + amplifier * sin (frequency * realToFrac t)) 0 0 0)
             (V4 0 (min + amplifier * sin (frequency * realToFrac t)) 0 0)
             (V4 0 0 (min + amplifier * sin (frequency * realToFrac t)) 0)
             (V4 0 0 0 1)
       )

-- | convolute applies a convolution effect where an object appears to be
-- wrapping up into itself.
convolute :: Behaviour
convolute t o =
  o & scale
    %~ ( !+!
           V4
             (V4 (0.5 * (sin . realToFrac $ t)) 0 0 0)
             (V4 0 (0.5 * (cos . realToFrac $ t)) 0 0)
             (V4 0 0 (0.5 * (sin . realToFrac $ t)) 0)
             (V4 0 0 0 1)
       )

shake :: Float -> Float -> Float -> Behaviour
shake amp scale frequency t o =
  o & pos
    %~ ( ^+^
           V3
             ((amp / 100) * sin (scale * frequency * realToFrac t))
             ((amp / 100) * (- cos (scale * frequency * realToFrac t)))
             0
       )

circle :: Float -> Behaviour
circle frequency t o =
  o & pos
    %~ ( ^+^
           V3
             (sin (frequency * realToFrac t))
             (cos (frequency * realToFrac t))
             0
       )

moveTo :: V3 Float -> Behaviour
moveTo target t o = o & pos %~ lerp (realToFrac t) target

rotate :: Float -> Behaviour
rotate factor t o = o & orientation %~ \oq -> axisAngle (V3 0 0 (-1)) (deg2rad (realToFrac t * factor)) * oq
