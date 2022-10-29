{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Horture.EventSource.Random
  ( runStaticEffectRandomizer,
    runAnyEffectRandomizer,
    StaticEffectRandomizerEnv,
  )
where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Array.IO
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Text (Text, unpack)
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

type StaticEffectRandomizerEnv = Map.Map Text (Text, Effect)

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
  RandomizeEffect (AddGif fp _ _ _) -> newRandomGifWith fp
  RandomizeEffect AddScreenBehaviour {} -> newRandomScreenEffect
  RandomizeEffect (AddShaderEffect _ se) -> randomizeShaderEffect se
  RandomizeEffect AddRapidFire {} -> newRapidFireEffect
  RandomizeEffect Noop -> return Noop

newRapidFireEffect ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRapidFireEffect = do
  n <- (uniformRM' @Int) 6 16
  gfs <- mapM newRandomGif' [1 .. n]
  return $ AddRapidFire gfs
  where
    newRandomGif' ::
      (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
      Int ->
      Eff effs Effect
    newRandomGif' _ = do
      AddGif <$> randomFilePathFromMap
        <*> (Limited <$> uniformRM' 8 18)
        <*> ( V3
                <$> (randomM' <&> (sin . (* 48)))
                <*> (randomM' <&> (cos . (* 19)))
                <*> return 0
            )
        <*> (uniformRM' 0 3 >>= newRandomBehaviours)
    randomFilePathFromMap ::
      (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
      Eff effs GifIndex
    randomFilePathFromMap = do
      gifs <- ask @StaticEffectRandomizerEnv >>= return . Map.toList
      uniformRM' 0 (length gifs - 1) <&> unpack . fst . (gifs !!)

newRandomGifWith ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  FilePath ->
  Eff effs Effect
newRandomGifWith fp =
  AddGif fp
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
        then newRandomGif
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
  gfs <- mapM (const newRandomGif) [1 .. n]
  return $ AddRapidFire gfs

newRandomShaderEffect :: (LastMember IO effs) => Eff effs Effect
newRandomShaderEffect = AddShaderEffect <$> (Limited <$> uniformRM' 2 6) <*> newRandomShader

newRandomShader :: (LastMember IO effs) => Eff effs ShaderEffect
newRandomShader = uniformRM' 0 (length effs - 1) <&> (effs !!)
  where
    effs = enumFrom minBound

newRandomScreenEffect :: (LastMember IO effs) => Eff effs Effect
newRandomScreenEffect = AddScreenBehaviour <$> (Limited <$> uniformRM' 6 10) <*> newRandomScreenBehaviours 1

newRandomGif ::
  (Members '[Reader StaticEffectRandomizerListEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomGif =
  AddGif
    <$> (ask @StaticEffectRandomizerListEnv >>= \gifs -> uniformRM' 0 (length gifs - 1) <&> (gifs !!))
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
  moveTo' <- moveTo . V3 0 0 <$> ((+ (-1)) . (/ 1) . negate <$> randomM')
  pulse' <- newRandomPulse
  rotate' <- rotate <$> randomRM' (-50) 50
  take n . cycle <$> liftIO (shuffle [moveTo', shake', pulse', rotate', convolute])

newRandomBehaviours :: (LastMember IO effs) => Int -> Eff effs [Behaviour]
newRandomBehaviours n = do
  shake' <- newRandomShake
  moveTo' <- newRandomMoveTo
  pulse' <- newRandomPulse
  circle' <- newRandomCircle
  rotate' <- rotate <$> randomRM' (-200) 200
  take n . cycle <$> liftIO (shuffle [shake', moveTo', pulse', rotate', circle', convolute])

newRandomShake :: (LastMember IO effs) => Eff effs Behaviour
newRandomShake = shake <$> randomM' <*> uniformRM' 80 160 <*> randomM'

newRandomMoveTo :: (LastMember IO effs) => Eff effs Behaviour
newRandomMoveTo = moveTo <$> (V3 <$> randomM' <*> randomM' <*> (negate <$> randomM'))

newRandomPulse :: (LastMember IO effs) => Eff effs Behaviour
newRandomPulse = pulse <$> randomM' <*> randomM' <*> ((*) <$> uniformRM' 1 10 <*> randomM')

newRandomCircle :: (LastMember IO effs) => Eff effs Behaviour
newRandomCircle = circle <$> ((*) <$> randomM' <*> randomRM' 1 3)

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
