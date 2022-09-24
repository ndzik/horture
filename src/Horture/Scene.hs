module Horture.Scene
  ( apply,
    applyAll,
    Scene (..),
    purge,
  )
where

import Horture.Effect
import Horture.Object

-- Scene is the horture scene. It consists of the background plane, which is
-- the screen being hortured as well as transient multiple objects in an
-- independent overlay.
data Scene = Scene
  { _screen :: Object,
    _objects :: [Object]
  }

-- apply applies the given effect at the time given using the elapsed time
-- since the last frame to the scene.
apply :: Double -> Double -> Effect -> Scene -> Scene
apply timeNow dt Noop s = s
apply timeNow dt (AddGif t) s = addGif t timeNow dt s
apply timeNow dt ShakeIt s = s
apply timeNow dt ZoomIt s = s
apply timeNow dt FlipIt s = s
apply timeNow dt Rollercoaster s = s
apply timeNow dt BlazeIt s = s
apply timeNow dt Flashbang s = s

-- applyAll composes all given effects at the time given time using the time
-- since the last frame as a progression point.
applyAll :: [Effect] -> Double -> Double -> Scene -> Scene
applyAll effs timeNow dt s = foldr (apply timeNow dt) s (Noop : effs)

addGif :: GifType -> Double -> Double -> Scene -> Scene
addGif Ricardo timeNow _ s = s {_objects = o : _objects s}
  where
    o = defGif 0 timeNow (Limited 8)

-- purge purges the given scene using timenow by removing all transient objects
-- which died off.
purge :: Double -> Scene -> Scene
purge timeNow s = s {_objects = filter (isStillAlive timeNow) . _objects $ s}
