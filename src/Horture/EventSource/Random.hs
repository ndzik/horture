{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  RandomizeEffect Noop -> return Noop

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

newRandomBarrelShader ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomBarrelShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Barrel

newRandomBlurShader ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomBlurShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Blur

newRandomStitchShader ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomStitchShader = AddShaderEffect <$> (Limited <$> uniformRM' 6 12) <*> return Stitch

newRandomFlashbangShader ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomFlashbangShader = AddShaderEffect <$> (Limited <$> uniformRM' 1 1) <*> return Flashbang

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
    if r < 0.4
      then newRandomGif
      else
        if r < 0.6
          then newRandomScreenEffect
          else newRandomShaderEffect

newRandomShaderEffect :: (LastMember IO effs) => Eff effs Effect
newRandomShaderEffect = AddShaderEffect <$> (Limited <$> uniformRM' 2 6) <*> newRandomShader

newRandomShader :: (LastMember IO effs) => Eff effs ShaderEffect
newRandomShader = uniformRM' 0 (length effs - 1) <&> (effs !!)
  where
    effs = [Barrel, Blur, Stitch, Flashbang]

newRandomScreenEffect :: (LastMember IO effs) => Eff effs Effect
newRandomScreenEffect = AddScreenBehaviour <$> (Limited <$> uniformRM' 6 10) <*> newRandomBehaviours 1

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

newRandomBehaviours :: (LastMember IO effs) => Int -> Eff effs [Behaviour]
newRandomBehaviours n = do
  shake' <- newRandomShake
  moveTo' <- newRandomMoveTo
  pulse' <- newRandomPulse
  circle' <- newRandomCircle
  take n . cycle <$> liftIO (shuffle [shake', moveTo', pulse', circle', convolute])

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
