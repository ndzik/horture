module Horture.Scene where

import Control.Lens
import Data.Default
import Data.Foldable (Foldable (foldr'))
import qualified Data.Map.Strict as Map
import Horture.Effect
import Horture.Asset
import Horture.Object
import Horture.Audio.Player
import Linear.V3
import Linear.V4

-- Scene is the horture scene. It consists of the background plane, which is
-- the screen being hortured as well as transient multiple objects in an
-- independent overlay.
data Scene = Scene
  { _screen :: !Object,
    _assets :: !(Map.Map AssetIndex [ActiveAsset]),
    _assetCache :: !(Map.Map FilePath HortureAsset),
    _shaders :: ![(ShaderEffect, Double, Lifetime)],
    _audio :: ![Sound StaticSoundEffect]
  }

instance Default Scene where
  def =
    Scene
      { _screen = def,
        _assets = Map.empty,
        _assetCache = Map.empty,
        _shaders = [],
        _audio = []
      }

-- ActiveAsset is an Asset which is about to be or currently acting in a scene.
data ActiveAsset = ActiveAsset
  { _afAsset :: !HortureAsset,
    _afObject :: !Object
  }

makeLenses ''Scene
makeLenses ''ActiveAsset

-- apply applies the given effect at the time given using the elapsed time
-- since the last frame to the scene.
apply :: Double -> Double -> Effect -> Scene -> Scene
apply _timeNow _dt Noop s = s
apply timeNow _dt (AddAsset i lt pos bs) s = addGif i timeNow lt (zip3 bs (repeat timeNow) (repeat lt)) pos s
apply timeNow _dt (AddScreenBehaviour lt bs) s = addScreenBehaviour timeNow lt (zip3 bs (repeat timeNow) (repeat lt)) s
apply timeNow _dt (AddShaderEffect lt eff audio) s = addAudio audio . addShaderEffect timeNow lt eff $ s
apply timeNow dt (AddRapidFire effs) s = foldr' (apply timeNow dt) s effs

addAudio :: [Sound StaticSoundEffect] -> Scene -> Scene
addAudio audioL s = s & audio %~ (++audioL)

-- applyAll composes all given effects at the given time using the time
-- since the last frame as a progression point.
applyAll :: [Effect] -> Double -> Double -> Scene -> Scene
applyAll effs timeNow dt s = foldr (apply timeNow dt) s (Noop : effs)

addGif :: AssetIndex -> Double -> Lifetime -> [(Behaviour, Double, Lifetime)] -> V3 Float -> Scene -> Scene
addGif i timeNow lt bs pos s =
  let loadedAssets = _assetCache s
      hAsset = Map.lookup i loadedAssets
      newAsset =
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
   in case hAsset of
        Nothing -> s
        Just hasset -> s {_assets = Map.insertWith (++) i [ActiveAsset hasset newAsset] . _assets $ s}

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
  let s = scene & assets %~ Map.map (filter (\(ActiveAsset _ o) -> isStillAlive timeNow (o ^. lifetime) (o ^. birth)))
      s' = s & screen . behaviours %~ filter (\(_, tob, lt) -> isStillAlive timeNow lt tob)
      s'' = s' & shaders %~ filter (\(_, tob, lt) -> isStillAlive timeNow lt tob)
      s''' = s'' & audio .~ []
   in s'''
