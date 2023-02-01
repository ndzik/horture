{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Horture.Audio.Player.Horture where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Default
import Control.Lens
import Horture.Audio.Player.Player
import Horture.Audio.Player.Protea
import Horture.Audio.Player.Effects
import Horture.Horture
import qualified Data.Map.Strict as Map
import Sound.ProteaAudio.SDL as Protea (Sample, sampleFromFile)
import Horture.Error
import Horture.State
import Horture.Logging

instance (HortureLogger (Horture hdl l)) => AudioPlayer (Horture hdl l) where
  initAudio = initHortureAudio
  deinitAudio = deinitProteaAudio
  clearAudio = clearProteaAudio
  playAudio = playHortureAudio

sampleFromFile' :: FilePath -> Float -> Horture l hdl Sample
sampleFromFile' fp = liftIO . sampleFromFile fp

initHortureAudio :: Horture hdl l ()
initHortureAudio = do
    liftIO (runProteaPlayer def def initProteaAudio) >>= \case
                  (Left _,_) -> throwError AudioSinkInitializationErr
                  (Right _,_) -> return ()
    env <- asks (^. audioEnv)
    let files = Map.toList $ staticSoundFiles env
    soundSamplesFiles <- mapM (\(n, fp) -> (n,) <$> sampleFromFile' fp 0.6) files
    soundSamplesGenerated <- mapM (\(n, pcm) -> (n,) <$> generateSampleFromPCM pcm) [(FlashbangPeep, flashbangPeep)]
    modify $ \s -> s & audioState %~ \as -> as { staticSounds = Map.fromList $ soundSamplesFiles ++ soundSamplesGenerated }

playHortureAudio :: Sound StaticSoundEffect -> Horture l hdl ()
playHortureAudio a = do
    as <- gets (^. audioState)
    ae <- asks (^. audioEnv)
    (res, as') <- liftIO $ runProteaPlayer ae as (playProteaAudio a)
    case res of
      Left err -> throwError $ AudioSinkPlayErr err
      Right _ -> pure ()
    modify (\s -> s & audioState .~ as')
