-- | This module defines some behaviours which can be used to modify how an
-- object is displayed during its lifetime.
module Horture.Behaviour
  ( pulse,
    circle,
    shake,
    moveTo,
    rotate,
    audiophile,
    bob,
    bounce,
    sway,
    wobble,
    spiralTo,
    orbit,
    breathe,
    jitter,
    easeTo,
    flipXPulse,
    identityDelta,
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

identityDelta :: BehaviourDelta
identityDelta = BehaviourDelta (V3 0 0 0) (V3 1 1 1) (axisAngle (V3 0 0 1) 0)

-- | pulse applies a pulsing effect, similar to a heartbeat.
pulse :: Float -> Float -> Float -> Behaviour
pulse min amplifier frequency =
  Behaviour BehaviourPulse $ \_ t _o ->
    let s = min + amplifier * sin (frequency * realToFrac t)
     in identityDelta {bdScale = V3 s s s}

shake :: Float -> Float -> Float -> Behaviour
shake amp scale frequency =
  Behaviour BehaviourShake $ \_ t _o ->
    let dx = (amp / 100) * sin (scale * frequency * realToFrac t)
        dy = (amp / 100) * (-cos (scale * frequency * realToFrac t))
     in identityDelta {bdPos = V3 dx dy 0}

circle :: Float -> Float -> Behaviour
circle scale frequency =
  Behaviour BehaviourCircle $ \_ t _o ->
    let x = scale * sin (frequency * realToFrac t)
        y = scale * cos (frequency * realToFrac t)
     in identityDelta {bdPos = V3 x y 0}

-- Move smoothly towards target over normalized time t \in [0,1]
moveTo :: V3 Float -> Behaviour
moveTo target =
  Behaviour BehaviourMoveTo $ \_ t o ->
    let p0 = o ^. pos
        p = lerp (realToFrac t) p0 target
        dp = p ^-^ p0
     in identityDelta {bdPos = dp}

-- Small incremental rotation each step (about -Z)
rotate :: Float -> Behaviour
rotate factor =
  Behaviour BehaviourRotate $ \_ _ _o ->
    identityDelta {bdRot = axisAngle (V3 0 0 (-1)) (deg2rad factor)}

audiophile :: Behaviour
audiophile =
  Behaviour BehaviourAudiophile $ \(bass, _, _) _ _o ->
    let fb = clamp (realToFrac bass) 0 1
        k = 0.15 -- max ~15% shrink
        s = 1 - k * fb
     in identityDelta {bdScale = V3 s s 1}
  where
    clamp x lo hi = max lo (min hi x)

bob :: Float -> Float -> Behaviour
bob amp freq = Behaviour BehaviourBob $ \_ t _o ->
  let y = amp * sin (freq * realToFrac t)
   in identityDelta {bdPos = V3 0 y 0}

bounce :: Float -> Float -> Behaviour
bounce amp freq = Behaviour BehaviourBounce $ \_ t _o ->
  let y = amp * abs (sin (freq * realToFrac t))
   in identityDelta {bdPos = V3 0 y 0}

sway :: Float -> Float -> Behaviour
sway degrees freq = Behaviour BehaviourSway $ \_ t _o ->
  let a = deg2rad (degrees * sin (freq * realToFrac t))
   in identityDelta {bdRot = axisAngle (V3 0 0 (-1)) a}

wobble :: Float -> Float -> Float -> Behaviour
wobble rotDeg scaleAmp freq = Behaviour BehaviourWobble $ \_ t _o ->
  let s = 1 + scaleAmp * sin (freq * realToFrac t)
      sx = s
      sy = 2 - s
      a = deg2rad (rotDeg * sin (0.5 * freq * realToFrac t))
   in identityDelta
        { bdScale = V3 sx sy 1,
          bdRot = axisAngle (V3 0 0 (-1)) a
        }

spiralTo :: V3 Float -> Float -> Float -> Behaviour
spiralTo target tightness turnsPerSec = Behaviour BehaviourSpiralTo $ \_ t o ->
  let tt = realToFrac t
      d = max 0 (1 - tightness * tt)
      ang = 2 * pi * turnsPerSec * tt
      off = V3 (d * cos ang) (d * sin ang) 0
      p0 = o ^. pos
      p1 = lerp (min 1 (tt * tightness)) p0 (target + off)
   in identityDelta {bdPos = p1 ^-^ p0}

orbit :: V3 Float -> Float -> Float -> Behaviour
orbit center radius freq = Behaviour BehaviourOrbit $ \_ t _o ->
  let a = freq * realToFrac t
      p = center ^+^ V3 (radius * cos a) (radius * sin a) 0
   in identityDelta {bdPos = p ^-^ center} -- delta relative to center; or use absolute apply path

breathe :: Float -> Float -> Behaviour
breathe amp freq = Behaviour BehaviourBreathe $ \_ t _o ->
  let s = 1 + amp * 0.5 * (sin (freq * realToFrac t) + sin (0.5 * freq * realToFrac t))
   in identityDelta {bdScale = V3 s s 1}

jitter :: Float -> Float -> Behaviour
jitter amp freq = Behaviour BehaviourJitter $ \_ t _o ->
  let tt = realToFrac t
      jx = amp * sin (freq * tt) + 0.5 * amp * sin (1.73 * freq * tt)
      jy = amp * cos (0.89 * freq * tt) + 0.5 * amp * sin (1.41 * freq * tt)
   in identityDelta {bdPos = V3 jx jy 0}

easeTo :: V3 Float -> Float -> Behaviour
easeTo target tau = Behaviour BehaviourEaseTo $ \_ t o ->
  let a = 1 - exp (-(realToFrac t) / tau)
      p0 = o ^. pos
      p1 = lerp a p0 target
   in identityDelta {bdPos = p1 ^-^ p0}

flipXPulse :: Float -> Behaviour
flipXPulse freq = Behaviour BehaviourFlipX $ \_ t _o ->
  let s = if sin (freq * realToFrac t) >= 0 then 1 else (-1)
   in identityDelta {bdScale = V3 s 1 1}
