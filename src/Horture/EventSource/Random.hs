{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Horture.EventSource.Random
  ( runStaticEffectRandomizer,
    runAnyEffectRandomizer,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
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
newRandomEffect = newRandomGif

{-# ANN newRandomGif ("HLint: ignore" :: String) #-}
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
