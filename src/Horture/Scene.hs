module Horture.Scene
  ( apply,
    applyAll,
    Scene (..),
    addGif,
    purge,
  )
where

import Horture.Effect
import Horture.Object
import Linear.V3

-- Scene is the horture scene. It consists of the background plane, which is
-- the screen being hortured as well as transient multiple objects in an
-- independent overlay.
data Scene = Scene
  { _screen :: Object,
    _gifs :: [Object]
  }

-- apply applies the given effect at the time given using the elapsed time
-- since the last frame to the scene.
apply :: Double -> Double -> Effect -> Scene -> Scene
apply _timeNow _dt Noop s = s
apply timeNow _dt (AddGif t lt pos) s = addGif t timeNow lt pos s
apply _timeNow _dt ShakeIt s = s
apply _timeNow _dt ZoomIt s = s
apply _timeNow _dt FlipIt s = s
apply _timeNow _dt Rollercoaster s = s
apply _timeNow _dt BlazeIt s = s
apply _timeNow _dt Flashbang s = s

-- applyAll composes all given effects at the time given time using the time
-- since the last frame as a progression point.
applyAll :: [Effect] -> Double -> Double -> Scene -> Scene
applyAll effs timeNow dt s = foldr (apply timeNow dt) s (Noop : effs)

addGif :: GifType -> Double -> Lifetime -> V3 Float -> Scene -> Scene
addGif Ricardo timeNow lt pos s = s {_gifs = o : _gifs s}
  where
    o = (defGif 0 timeNow lt 20 2) {_pos = pos}

-- purge purges the given scene using timenow by removing all transient objects
-- which died off.
purge :: Double -> Scene -> Scene
purge timeNow s = s {_gifs = filter (isStillAlive timeNow) . _gifs $ s}
