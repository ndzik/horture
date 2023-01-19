{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
  RandomizeEffect (AddAsset "" _ _ _) -> newRandomAssetAny
  RandomizeEffect (AddAsset fp _ _ _) -> newRandomAssetWith fp
  RandomizeEffect (AddScreenBehaviour _ [Behaviour bht _]) -> case bht of
    BehaviourPulse -> mkRandomScreenEffect <*> ((: []) <$> newRandomPulseScreen)
    BehaviourConvolute -> mkRandomScreenEffect <*> ((: []) <$> newRandomConvolute)
    BehaviourShake -> mkRandomScreenEffect <*> ((: []) <$> newRandomShake)
    BehaviourCircle -> mkRandomScreenEffect <*> ((: []) <$> newRandomCircleScreen)
    BehaviourMoveTo -> mkRandomScreenEffect <*> ((: []) <$> newRandomMoveToScreen)
    BehaviourRotate -> mkRandomScreenEffect <*> ((: []) <$> newRandomRotate)
    BehaviourAudiophile -> mkRandomScreenEffect <*> ((: []) <$> newRandomAudiophile)
  RandomizeEffect AddScreenBehaviour {} -> newRandomScreenEffect
  RandomizeEffect (AddShaderEffect _ se) -> randomizeShaderEffect se
  RandomizeEffect AddRapidFire {} -> newRapidFireEffect
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

newRandomAudioShader :: (LastMember IO effs) => Eff effs Effect
newRandomAudioShader = AddShaderEffect <$> (Limited <$> uniformRM' 26 36) <*> return Audiophile

newRandomToonShader :: (LastMember IO effs) => Eff effs Effect
newRandomToonShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Toonify

newRandomInvertShader :: (LastMember IO effs) => Eff effs Effect
newRandomInvertShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Invert

newRandomMirrorShader :: (LastMember IO effs) => Eff effs Effect
newRandomMirrorShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Mirror

newRandomBarrelShader :: (LastMember IO effs) => Eff effs Effect
newRandomBarrelShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Barrel

newRandomBlurShader :: (LastMember IO effs) => Eff effs Effect
newRandomBlurShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Blur

newRandomStitchShader :: (LastMember IO effs) => Eff effs Effect
newRandomStitchShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Stitch

newRandomFlashbangShader :: (LastMember IO effs) => Eff effs Effect
newRandomFlashbangShader = AddShaderEffect <$> (Limited <$> uniformRM' 1 3) <*> return Flashbang

newRandomCycleShader :: (LastMember IO effs) => Eff effs Effect
newRandomCycleShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Cycle

newRandomBlinkShader :: (LastMember IO effs) => Eff effs Effect
newRandomBlinkShader = AddShaderEffect <$> (Limited <$> uniformRM' 1 3) <*> return Blink

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
    if r < 0.3
      then newRandomAsset
      else
        if r < 0.5
          then newRandomScreenEffect
          else
            if r < 0.8
              then newRandomShaderEffect
              else newRandomRapidFireEffect

newRandomRapidFireEffect ::
  (Members '[Reader StaticEffectRandomizerListEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomRapidFireEffect = do
  n <- (uniformRM' @Int) 6 16
  gfs <- mapM (const newRandomAsset) [1 .. n]
  return $ AddRapidFire gfs

newRandomShaderEffect :: (LastMember IO effs) => Eff effs Effect
newRandomShaderEffect = AddShaderEffect <$> (Limited <$> uniformRM' 2 6) <*> newRandomShader

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
    <*> (Limited <$> uniformRM' 8 18)
    <*> ( V3
            <$> (randomM' <&> (sin . (* 20)))
            <*> (randomM' <&> (cos . (* 33)))
            <*> return 0
        )
    <*> (uniformRM' 0 3 >>= newRandomBehaviours)

newRandomScreenBehaviours :: (LastMember IO effs) => Int -> Eff effs [Behaviour]
newRandomScreenBehaviours n = do
  shake' <- newRandomShake
  moveTo' <- newRandomMoveToScreen
  pulse' <- newRandomPulse
  rotate' <- newRandomRotate
  take n . cycle <$> liftIO (shuffle [moveTo', shake', pulse', rotate', convolute, audiophile])

newRandomMoveToScreen :: (LastMember IO effs) => Eff effs Behaviour
newRandomMoveToScreen = moveTo . V3 0 0 <$> ((+ (-1)) . (/ 1) . negate <$> randomM')

newRandomBehaviours :: (LastMember IO effs) => Int -> Eff effs [Behaviour]
newRandomBehaviours n = do
  shake' <- newRandomShake
  moveTo' <- newRandomMoveTo
  pulse' <- newRandomPulse
  circle' <- newRandomCircle
  rotate' <- newRandomRotate
  take n . cycle <$> liftIO (shuffle [shake', moveTo', pulse', rotate', circle', convolute, audiophile])

newRandomShake :: (LastMember IO effs) => Eff effs Behaviour
newRandomShake = shake <$> randomM' <*> uniformRM' 80 160 <*> randomM'

newRandomConvolute :: (LastMember IO effs) => Eff effs Behaviour
newRandomConvolute = return convolute

newRandomRotate :: (LastMember IO effs) => Eff effs Behaviour
newRandomRotate = rotate <$> randomRM' (-1) 1

newRandomMoveTo :: (LastMember IO effs) => Eff effs Behaviour
newRandomMoveTo = moveTo <$> (V3 <$> randomM' <*> randomM' <*> (negate <$> randomM'))

newRandomPulseScreen :: (LastMember IO effs) => Eff effs Behaviour
newRandomPulseScreen = pulse 1 (1 / 100) <$> uniformRM' 20 200

newRandomPulse :: (LastMember IO effs) => Eff effs Behaviour
newRandomPulse = pulse <$> randomM' <*> randomM' <*> ((*) <$> uniformRM' 1 10 <*> randomM')

newRandomCircle :: (LastMember IO effs) => Eff effs Behaviour
newRandomCircle = circle 1 <$> ((*) <$> randomM' <*> randomRM' 1 3)

newRandomCircleScreen :: (LastMember IO effs) => Eff effs Behaviour
newRandomCircleScreen = circle (1 / 10) <$> uniformRM' 20 30

newRandomAudiophile :: (LastMember IO effs) => Eff effs Behaviour
newRandomAudiophile = return audiophile

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
