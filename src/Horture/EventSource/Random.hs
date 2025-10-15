module Horture.EventSource.Random
  ( runStaticEffectRandomizer,
    runAnyEffectRandomizer,
    StaticEffectRandomizerEnv (..),
    assetEffects,
    registeredEffects,
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Array.IO
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Horture.Audio.Player
import Horture.Behaviour
import Horture.Effect
import Horture.EventSource.EventSource
import Horture.Object
import Linear.V3
import System.Random
import System.Random.Stateful
  ( UniformRange (uniformRM),
    globalStdGen,
    randomM,
    randomRM,
  )

data StaticEffectRandomizerEnv = StaticEffectRandomizerEnv
  { _registeredEffects :: !(Map.Map Text (Text, Effect)),
    _assetEffects :: ![FilePath]
  }

makeLenses ''StaticEffectRandomizerEnv

type StaticEffectRandomizerListEnv = [FilePath]

-- | Static effect randomizer. It is static because the randomization of some
-- effects depend on external configuration. This configuration is fixed here.
-- For a version which allows to customize the randomized effects
-- `runDynamicEffectRandomizer` can be used.
runStaticEffectRandomizer ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff (RandomizeEffect : effs) x ->
  Eff effs x
runStaticEffectRandomizer = interpret $ \case
  RandomizeEffect (AddAsset "" _ _ _) -> return Noop -- newRandomAssetAny
  RandomizeEffect (AddAsset fp _ _ _) -> return Noop -- newRandomAssetWith fp
  RandomizeEffect (AddScreenBehaviour _ [Behaviour bht _]) -> case bht of
    BehaviourPulse -> mkRandomScreenEffect <*> ((: []) <$> newRandomPulseScreen)
    BehaviourShake -> mkRandomScreenEffect <*> ((: []) <$> newRandomShake)
    BehaviourCircle -> mkRandomScreenEffect <*> ((: []) <$> newRandomCircleScreen)
    BehaviourMoveTo -> mkRandomScreenEffect <*> ((: []) <$> newRandomMoveToScreen)
    BehaviourRotate -> mkRandomScreenEffect <*> ((: []) <$> newRandomRotate)
    BehaviourAudiophile -> mkRandomScreenEffect <*> ((: []) <$> newRandomAudiophile)
    BehaviourFlipX -> mkRandomScreenEffect <*> ((: []) <$> newRandomFlipX)
    BehaviourEaseTo -> mkRandomScreenEffect <*> ((: []) <$> newRandomEaseTo)
    BehaviourJitter -> mkRandomScreenEffect <*> ((: []) <$> newRandomJitter)
    BehaviourBreathe -> mkRandomScreenEffect <*> ((: []) <$> newRandomBreathe)
    BehaviourOrbit -> mkRandomScreenEffect <*> ((: []) <$> newRandomOrbit)
    BehaviourSpiralTo -> mkRandomScreenEffect <*> ((: []) <$> newRandomSpiralTo)
    BehaviourWobble -> mkRandomScreenEffect <*> ((: []) <$> newRandomWobble)
    BehaviourSway -> mkRandomScreenEffect <*> ((: []) <$> newRandomSway)
    BehaviourBounce -> mkRandomScreenEffect <*> ((: []) <$> newRandomBounce)
    BehaviourBob -> mkRandomScreenEffect <*> ((: []) <$> newRandomBob)
  RandomizeEffect AddScreenBehaviour {} -> newRandomScreenEffect
  RandomizeEffect (AddShaderEffect _ se _) -> randomizeShaderEffect se
  RandomizeEffect AddRapidFire {} -> return Noop -- newRapidFireEffect
  RandomizeEffect (RemoveShaderEffect _) -> RemoveShaderEffect <$> randomM'
  RandomizeEffect (RemoveScreenBehaviour _) -> RemoveScreenBehaviour <$> randomM'
  RandomizeEffect Noop -> return Noop

newRapidFireEffect ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRapidFireEffect = do
  asks @StaticEffectRandomizerEnv (^. assetEffects) >>= \case
    [] -> return Noop
    assets -> do
      n <- (uniformRM' @Int) 6 16
      gfs <- mapM (newRandomAsset' assets) [1 .. n]
      return $ AddRapidFire gfs
  where
    newRandomAsset' ::
      (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
      [FilePath] ->
      Int ->
      Eff effs Effect
    newRandomAsset' assets _ = randomFilePath assets >>= newRandomAssetWith
    randomFilePath ::
      (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
      [FilePath] ->
      Eff effs AssetIndex
    randomFilePath assets = uniformRM' 0 (length assets - 1) <&> (assets !!)

newRandomAssetAny ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomAssetAny = do
  asks (^. assetEffects) >>= \case
    [] -> return Noop
    assets -> newRandomAssetAny' assets

newRandomAssetWith ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  FilePath ->
  Eff effs Effect
newRandomAssetWith fp =
  AddAsset fp
    <$> (Limited <$> uniformRM' 8 18)
    <*> ( V3
            <$> (randomM' <&> (sin . (* 20)))
            <*> (randomM' <&> (cos . (* 33)))
            <*> return 0
        )
    <*> (uniformRM' 0 3 >>= newRandomBehaviours)

randomizeShaderEffect ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  ShaderEffect ->
  Eff effs Effect
randomizeShaderEffect Barrel = newRandomBarrelShader
randomizeShaderEffect Blur = newRandomBlurShader
randomizeShaderEffect Stitch = newRandomStitchShader
randomizeShaderEffect Flashbang = newRandomFlashbangShader
randomizeShaderEffect Cycle = newRandomCycleShader
randomizeShaderEffect Blink = newRandomBlinkShader
randomizeShaderEffect Mirror = newRandomMirrorShader
randomizeShaderEffect Invert = newRandomInvertShader
randomizeShaderEffect Toonify = newRandomToonShader
randomizeShaderEffect Audiophile = newRandomAudioShader
randomizeShaderEffect BassRealityWarp = newRandomBassRealityWarpShader
randomizeShaderEffect Glitch = newRandomGlitchShader
randomizeShaderEffect Kaleidoscope = newRandomKaleidoscopeShader

newRandomKaleidoscopeShader :: (LastMember IO effs) => Eff effs Effect
newRandomKaleidoscopeShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 26 36)
    <*> return Kaleidoscope
    <*> return []

newRandomGlitchShader :: (LastMember IO effs) => Eff effs Effect
newRandomGlitchShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 26 36)
    <*> return Glitch
    <*> return []

newRandomAudioShader :: (LastMember IO effs) => Eff effs Effect
newRandomAudioShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 26 36)
    <*> return Audiophile
    <*> return []

newRandomBassRealityWarpShader :: (LastMember IO effs) => Eff effs Effect
newRandomBassRealityWarpShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 26 36)
    <*> return BassRealityWarp
    <*> return []

newRandomToonShader :: (LastMember IO effs) => Eff effs Effect
newRandomToonShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 6 12)
    <*> return Toonify
    <*> return []

newRandomInvertShader :: (LastMember IO effs) => Eff effs Effect
newRandomInvertShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 6 12)
    <*> return Invert
    <*> return []

newRandomMirrorShader :: (LastMember IO effs) => Eff effs Effect
newRandomMirrorShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 6 12)
    <*> return Mirror
    <*> return []

newRandomBarrelShader :: (LastMember IO effs) => Eff effs Effect
newRandomBarrelShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 6 12)
    <*> return Barrel
    <*> return []

newRandomBlurShader :: (LastMember IO effs) => Eff effs Effect
newRandomBlurShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 6 12)
    <*> return Blur
    <*> return []

newRandomStitchShader :: (LastMember IO effs) => Eff effs Effect
newRandomStitchShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 6 12)
    <*> return Stitch
    <*> return []

newRandomFlashbangShader :: (LastMember IO effs) => Eff effs Effect
newRandomFlashbangShader = do
  pitchPeep <- randomRM' 0.1 0.6
  pitchBang <- randomRM' 0.7 1.2
  AddShaderEffect
    <$> (Limited <$> uniformRM' 1 3)
    <*> return Flashbang
    <*> return
      [ StaticSound pitchPeep FlashbangPeep,
        StaticSound pitchBang FlashbangBang
      ]

newRandomCycleShader :: (LastMember IO effs) => Eff effs Effect
newRandomCycleShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 6 12)
    <*> return Cycle
    <*> return []

newRandomBlinkShader :: (LastMember IO effs) => Eff effs Effect
newRandomBlinkShader =
  AddShaderEffect
    <$> (Limited <$> uniformRM' 1 3)
    <*> return Blink
    <*> return []

-- | Generate a random value uniformly distributed over the given range.
uniformRM' :: (UniformRange a, LastMember IO effs) => a -> a -> Eff effs a
uniformRM' lb ub = liftIO $ uniformRM (lb, ub) globalStdGen

randomRM' :: (Random a, LastMember IO effs) => a -> a -> Eff effs a
randomRM' lb ub = liftIO $ randomRM (lb, ub) globalStdGen

-- | Generate a pseudo-random value using its `Random` instance.
randomM' :: (LastMember IO effs, Random a) => Eff effs a
randomM' = liftIO (randomM globalStdGen)

-- | Completely ignores which effect is inputted and randomizes from every
-- possible effect.
runAnyEffectRandomizer ::
  (Members '[Reader StaticEffectRandomizerListEnv] effs, LastMember IO effs) =>
  Eff (RandomizeEffect : effs) x ->
  Eff effs x
runAnyEffectRandomizer = interpret $ \case
  RandomizeEffect _ -> newRandomEffect

newRandomEffect ::
  (Members '[Reader StaticEffectRandomizerListEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomEffect =
  randomM' @_ @Float >>= \r ->
    let -- effects = [newRandomAsset, newRandomScreenEffect, newRandomShaderEffect, newRandomPurgeEffect, newRandomRapidFireEffect]
        effects = [newRandomAsset, newRandomScreenEffect, newRandomShaderEffect, newRandomPurgeEffect]
        n = length effects
        idx = floor (r * fromIntegral n) `mod` n
     in effects !! idx

newRandomPurgeEffect ::
  (Members '[Reader StaticEffectRandomizerListEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomPurgeEffect = do
  randomM' @_ @Float >>= \r ->
    if r < 0.5
      then RemoveScreenBehaviour <$> randomM'
      else RemoveShaderEffect <$> randomM'

newRandomRapidFireEffect ::
  (Members '[Reader StaticEffectRandomizerListEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomRapidFireEffect = do
  n <- (uniformRM' @Int) 6 16
  gfs <- mapM (const newRandomAsset) [1 .. n]
  return $ AddRapidFire gfs

newRandomShaderEffect :: (LastMember IO effs) => Eff effs Effect
newRandomShaderEffect = AddShaderEffect <$> (Limited <$> uniformRM' 2 6) <*> newRandomShader <*> return []

newRandomShader :: (LastMember IO effs) => Eff effs ShaderEffect
newRandomShader = uniformRM' 0 (length effs - 1) <&> (effs !!)
  where
    effs = enumFrom minBound

mkRandomScreenEffect :: (LastMember IO effs) => Eff effs ([Behaviour] -> Effect)
mkRandomScreenEffect = AddScreenBehaviour <$> (Limited <$> uniformRM' 6 10)

newRandomScreenEffect :: (LastMember IO effs) => Eff effs Effect
newRandomScreenEffect = mkRandomScreenEffect <*> newRandomScreenBehaviours 1

newRandomAsset ::
  (Members '[Reader StaticEffectRandomizerListEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomAsset =
  ask @StaticEffectRandomizerListEnv >>= \case
    [] -> return Noop
    assets -> newRandomAssetAny' assets

newRandomAssetAny' :: (LastMember IO effs) => [FilePath] -> Eff effs Effect
newRandomAssetAny' assets = do
  AddAsset
    <$> (uniformRM' 0 (length assets - 1) <&> (assets !!))
    <*> (Limited <$> uniformRM' 4 18)
    <*> ( V3
            <$> (randomM' <&> (sin . (* 20)))
            <*> (randomM' <&> (cos . (* 33)))
            <*> return 0
        )
    <*> (uniformRM' 0 16 >>= newRandomBehaviours)

newRandomScreenBehaviours :: (LastMember IO effs) => Int -> Eff effs [Behaviour]
newRandomScreenBehaviours n = do
  shake' <- newRandomShake
  moveTo' <- newRandomMoveTo
  pulse' <- newRandomPulse
  circle' <- newRandomCircle
  rotate' <- newRandomRotate
  audiophile <- newRandomAudiophile
  orbit' <- newRandomOrbit
  spiral' <- newRandomSpiralTo
  wobble' <- newRandomWobble
  sway' <- newRandomSway
  bounce' <- newRandomBounce
  bob' <- newRandomBob
  flipX' <- newRandomFlipX
  easeTo' <- newRandomEaseTo
  jitter' <- newRandomJitter
  breathe' <- newRandomBreathe
  take n . cycle
    <$> liftIO
      ( shuffle
          [ shake',
            moveTo',
            pulse',
            rotate',
            circle',
            audiophile,
            orbit',
            spiral',
            wobble',
            sway',
            bounce',
            bob',
            flipX',
            easeTo',
            jitter',
            breathe'
          ]
      )

newRandomMoveToScreen :: (LastMember IO effs) => Eff effs Behaviour
newRandomMoveToScreen = moveTo . V3 0 0 <$> ((+ (-1)) . (/ 1) . negate <$> randomM')

newRandomBehaviours :: (LastMember IO effs) => Int -> Eff effs [Behaviour]
newRandomBehaviours n = do
  shake' <- newRandomShake
  moveTo' <- newRandomMoveTo
  pulse' <- newRandomPulse
  circle' <- newRandomCircle
  rotate' <- newRandomRotate
  audiophile <- newRandomAudiophile
  orbit' <- newRandomOrbit
  spiral' <- newRandomSpiralTo
  wobble' <- newRandomWobble
  sway' <- newRandomSway
  bounce' <- newRandomBounce
  bob' <- newRandomBob
  flipX' <- newRandomFlipX
  easeTo' <- newRandomEaseTo
  jitter' <- newRandomJitter
  breathe' <- newRandomBreathe
  take n . cycle
    <$> liftIO
      ( shuffle
          [ shake',
            moveTo',
            pulse',
            rotate',
            circle',
            audiophile,
            orbit',
            spiral',
            wobble',
            sway',
            bounce',
            bob',
            flipX',
            easeTo',
            jitter',
            breathe'
          ]
      )

randX, randY, randZ :: (LastMember IO effs) => Eff effs Float
randX = uniformRM' (-0.25) 0.25
randY = uniformRM' (-0.15) 0.15
randZ = pure (-1) -- keep on plane

newRandomShake :: (LastMember IO effs) => Eff effs Behaviour
newRandomShake = shake <$> uniformRM' 0.01 0.04 <*> uniformRM' 0.8 1.3 <*> uniformRM' 2 6

newRandomRotate :: (LastMember IO effs) => Eff effs Behaviour
newRandomRotate = rotate <$> uniformRM' 0.05 0.5 -- deg per frame (~3–30°/s @60fps)

newRandomFlipX :: (LastMember IO effs) => Eff effs Behaviour
newRandomFlipX = flipXPulse <$> uniformRM' 0.2 1.0 -- flips per second

newRandomEaseTo :: (LastMember IO effs) => Eff effs Behaviour
newRandomEaseTo = easeTo <$> (V3 <$> randX <*> randY <*> randZ) <*> uniformRM' 0.6 2.5

newRandomJitter :: (LastMember IO effs) => Eff effs Behaviour
newRandomJitter = jitter <$> uniformRM' 0.005 0.02 <*> uniformRM' 6 14

newRandomBreathe :: (LastMember IO effs) => Eff effs Behaviour
newRandomBreathe = breathe <$> uniformRM' 0.03 0.10 <*> uniformRM' 0.2 0.6

newRandomOrbit :: (LastMember IO effs) => Eff effs Behaviour
newRandomOrbit =
  orbit
    <$> (V3 <$> randX <*> randY <*> randZ)
    <*> uniformRM' 0.05 0.15
    <*> uniformRM' 0.1 0.4

newRandomSpiralTo :: (LastMember IO effs) => Eff effs Behaviour
newRandomSpiralTo =
  spiralTo
    <$> (V3 <$> randX <*> randY <*> randZ)
    <*> uniformRM' 0.3 1.0
    <*> uniformRM' 0.1 0.5

newRandomWobble :: (LastMember IO effs) => Eff effs Behaviour
newRandomWobble =
  wobble
    <$> uniformRM' 2 8 -- rot deg peak
    <*> uniformRM' 0.05 0.20 -- scale amp
    <*> uniformRM' 0.5 2.0 -- Hz-ish

newRandomSway :: (LastMember IO effs) => Eff effs Behaviour
newRandomSway = sway <$> uniformRM' 3 15 <*> uniformRM' 0.25 1.5

newRandomBounce :: (LastMember IO effs) => Eff effs Behaviour
newRandomBounce = bounce <$> uniformRM' 0.02 0.10 <*> uniformRM' 0.6 1.8

newRandomBob :: (LastMember IO effs) => Eff effs Behaviour
newRandomBob = bob <$> uniformRM' 0.02 0.06 <*> uniformRM' 0.3 0.8

newRandomMoveTo :: (LastMember IO effs) => Eff effs Behaviour
newRandomMoveTo = moveTo <$> (V3 <$> randX <*> randY <*> randZ)

newRandomPulseScreen :: (LastMember IO effs) => Eff effs Behaviour
newRandomPulseScreen = pulse 1.0 (1 / 60) <$> uniformRM' 0.5 2.0 -- ~1–2% scale swing

newRandomPulse :: (LastMember IO effs) => Eff effs Behaviour
newRandomPulse =
  pulse
    <$> uniformRM' 0.9 1.1 -- base scale
    <*> uniformRM' 0.03 0.12 -- amp
    <*> uniformRM' 0.8 3.0 -- freq

newRandomCircle :: (LastMember IO effs) => Eff effs Behaviour
newRandomCircle = circle <$> uniformRM' 0.05 0.20 <*> uniformRM' 0.2 0.8

newRandomCircleScreen :: (LastMember IO effs) => Eff effs Behaviour
newRandomCircleScreen = circle <$> uniformRM' 0.01 0.03 <*> uniformRM' 0.3 0.8

newRandomAudiophile :: (LastMember IO effs) => Eff effs Behaviour
newRandomAudiophile = pure audiophile

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1, n) xs
