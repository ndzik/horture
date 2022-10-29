module Horture.Scene
  ( apply,
    applyAll,
    Scene (..),
    ActiveGif (..),
    addGif,
    purge,
    -- | Lenses.
    screen,
    gifs,
    gifCache,
    afGif,
    afObject,
    shaders,
  )
where

import Control.Lens
import Data.Default
import Data.Foldable (Foldable (foldr'))
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
    _gifs :: !(Map.Map GifIndex [ActiveGif]),
    _gifCache :: !(Map.Map FilePath HortureGif),
    _shaders :: ![(ShaderEffect, Double, Lifetime)]
  }

instance Default Scene where
  def =
    Scene
      { _screen = def,
        _gifs = Map.empty,
        _gifCache = Map.empty,
        _shaders = []
      }

-- ActiveGif is a GIF which is about or currently acting in a scene.
data ActiveGif = AGIF
  { _afGif :: !HortureGif,
    _afObject :: !Object
  }

makeLenses ''Scene
makeLenses ''ActiveGif

-- apply applies the given effect at the time given using the elapsed time
-- since the last frame to the scene.
apply :: Double -> Double -> Effect -> Scene -> Scene
apply _timeNow _dt Noop s = s
apply timeNow _dt (AddGif i lt pos bs) s = addGif i timeNow lt (zip3 bs (repeat timeNow) (repeat lt)) pos s
apply timeNow _dt (AddScreenBehaviour lt bs) s = addScreenBehaviour timeNow lt (zip3 bs (repeat timeNow) (repeat lt)) s
apply timeNow _dt (AddShaderEffect lt eff) s = addShaderEffect timeNow lt eff s
apply timeNow dt (AddRapidFire effs) s = foldr' (apply timeNow dt) s effs

-- applyAll composes all given effects at the given time using the time
-- since the last frame as a progression point.
applyAll :: [Effect] -> Double -> Double -> Scene -> Scene
applyAll effs timeNow dt s = foldr (apply timeNow dt) s (Noop : effs)

addGif :: GifIndex -> Double -> Lifetime -> [(Behaviour, Double, Lifetime)] -> V3 Float -> Scene -> Scene
addGif i timeNow lt bs pos s =
  let loadedGifs = _gifCache s
      hGif = Map.lookup i loadedGifs
      newGif =
        def
          { _pos = pos,
            _lifetime = lt,
            _birth = timeNow,
            _scale =
              V4
                (V4 0.33 0 0 0)
                (V4 0 0.33 0 0)
                (V4 0 0 0.33 0)
                (V4 0 0 0 1),
            _behaviours = bs
          }
   in case hGif of
        Nothing -> s
        Just hgif -> s {_gifs = Map.insertWith (++) i [AGIF hgif newGif] . _gifs $ s}

addScreenBehaviour :: Double -> Lifetime -> [(Behaviour, Double, Lifetime)] -> Scene -> Scene
addScreenBehaviour _timeNow _lt bs scene =
  let s = scene & screen . behaviours %~ (++ bs)
   in s

addShaderEffect :: Double -> Lifetime -> ShaderEffect -> Scene -> Scene
addShaderEffect timeNow lt eff scene =
  let s = scene & shaders %~ ((eff, timeNow, lt) :)
   in s

-- | purge purges the given scene using timenow by removing all transient
-- objects and effects that died off.
purge :: Double -> Scene -> Scene
purge timeNow scene =
  let s = scene & gifs %~ Map.map (filter (\(AGIF _ o) -> isStillAlive timeNow (o ^. lifetime) (o ^. birth)))
      s' = s & screen . behaviours %~ filter (\(_, tob, lt) -> isStillAlive timeNow lt tob)
      s'' = s' & shaders %~ filter (\(_, tob, lt) -> isStillAlive timeNow lt tob)
   in s''
