module Horture.Scene
  ( apply,
    applyAll,
    Scene (..),
    ActiveGIF (..),
    addGif,
    purge,
  )
where

import Data.Default
import qualified Data.Map.Strict as Map
import Horture.Effect
import Horture.Gif
import Horture.Object
import Linear.V3
import Linear.V4

-- Scene is the horture scene. It consists of the background plane, which is
-- the screen being hortured as well as transient multiple objects in an
-- independent overlay.
data Scene = Scene
  { _screen :: !Object,
    _gifs :: !(Map.Map GifIndex [ActiveGIF]),
    _gifCache :: !(Map.Map FilePath HortureGIF)
  }

-- ActiveGIF is a GIF which is about or currently acting in a scene.
data ActiveGIF = AGIF
  { _afGif :: !HortureGIF,
    _afObject :: !Object
  }
  deriving (Show)

-- apply applies the given effect at the time given using the elapsed time
-- since the last frame to the scene.
apply :: Double -> Double -> Effect -> Scene -> Scene
apply _timeNow _dt Noop s = s
apply timeNow _dt (AddGif i lt pos) s = addGif i timeNow lt pos s
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

addGif :: GifIndex -> Double -> Lifetime -> V3 Float -> Scene -> Scene
addGif i timeNow lt pos s =
  let loadedGifs = _gifCache s
      hGif = Map.lookup i loadedGifs
      newGif = def {_pos = pos, _lifetime = lt, _birth = timeNow, _scale = V4
                                                                            (V4 0.33 0 0 0)
                                                                            (V4 0 0.33 0 0)
                                                                            (V4 0 0 0.33 0)
                                                                            (V4 0 0 0 1)
                                                                            }
   in case hGif of
        Nothing -> s
        Just hgif -> s {_gifs = Map.insertWith (++) i [AGIF hgif newGif] . _gifs $ s}

-- purge purges the given scene using timenow by removing all transient objects
-- which died off.
purge :: Double -> Scene -> Scene
purge timeNow s = s {_gifs = Map.map (filter (isStillAlive timeNow . _afObject)) . _gifs $ s}
