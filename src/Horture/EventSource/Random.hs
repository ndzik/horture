{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Horture.EventSource.Random
  ( runStaticEffectRandomizer,
    runAnyEffectRandomizer,
  )
where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Array.IO
import Data.Functor ((<&>))
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
  )

type StaticEffectRandomizerEnv = [FilePath]

-- | Static effect randomizer. It is static because the randomization of some
-- effects depend on external configuration. This configuration is fixed here.
-- For a version which allows to customize the randomized effects
-- `runDynamicEffectRandomizer` can be used.
runStaticEffectRandomizer ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff (RandomizeEffect : effs) x ->
  Eff effs x
runStaticEffectRandomizer = interpret $ \case
  RandomizeEffect AddGif {} -> newRandomGif
  -- We will not touch other effects for now.
  RandomizeEffect eff -> return eff

-- | Generate a random value uniformly distributed over the given range.
uniformRM' :: (UniformRange a, LastMember IO effs) => a -> a -> Eff effs a
uniformRM' lb ub = liftIO $ uniformRM (lb, ub) globalStdGen

-- | Generate a pseudo-random value using its `Random` instance.
randomM' :: (LastMember IO effs, Random a) => Eff effs a
randomM' = liftIO (randomM globalStdGen)

-- | Completely ignores which effect is inputted and randomizes from every
-- possible effect.
runAnyEffectRandomizer ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff (RandomizeEffect : effs) x ->
  Eff effs x
runAnyEffectRandomizer = interpret $ \case
  -- TODO: Expand the pool of randomized effects.
  RandomizeEffect _ -> newRandomEffect

newRandomEffect ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomEffect = randomM' @_ @Float >>= \r -> if r < 0.5 then newRandomGif else newRandomScreenEffect

newRandomScreenEffect ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomScreenEffect = AddScreenBehaviour <$> (Limited <$> uniformRM' 6 10) <*> newRandomBehaviours 1

newRandomGif ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Effect
newRandomGif =
  AddGif
    <$> (ask @StaticEffectRandomizerEnv >>= \gifs -> uniformRM' 0 (length gifs - 1) <&> (gifs !!))
    <*> (Limited <$> uniformRM' 8 18)
    <*> ( V3
            <$> (randomM' <&> (sin . (* 20)))
            <*> (randomM' <&> (cos . (* 33)))
            <*> return 0
        )
    <*> (uniformRM' 0 3 >>= newRandomBehaviours)

newRandomBehaviours ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Int ->
  Eff effs [Behaviour]
newRandomBehaviours n = do
  shake' <- newRandomShake
  moveTo' <- newRandomMoveTo
  pulse' <- newRandomPulse
  circle' <- newRandomCircle
  take n . cycle <$> liftIO (shuffle [shake', moveTo', pulse', circle', convolute])

newRandomShake ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Behaviour
newRandomShake = shake <$> randomM' <*> uniformRM' 80 160 <*> randomM'

newRandomMoveTo ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Behaviour
newRandomMoveTo = moveTo <$> (V3 <$> randomM' <*> randomM' <*> (negate <$> randomM'))

newRandomPulse ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Behaviour
newRandomPulse = pulse <$> randomM' <*> randomM' <*> ((*) <$> uniformRM' 1 10 <*> randomM')

newRandomCircle ::
  (Members '[Reader StaticEffectRandomizerEnv] effs, LastMember IO effs) =>
  Eff effs Behaviour
newRandomCircle = circle <$> ((*) <$> randomM' <*> uniformRM' 1 10)

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
