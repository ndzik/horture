{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Horture.Audio.Player.Horture where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import qualified Data.Map.Strict as Map
import Horture.Audio.Player.Effects
import Horture.Audio.Player.Player
import Horture.Audio.Player.Protea
import Horture.Error
import Horture.Horture
import Horture.Logging
import Horture.State
import Sound.ProteaAudio.SDL as Protea (Sample, sampleFromFile)
import UnliftIO.Exception (bracket)

instance (HortureLogger (Horture hdl l)) => AudioPlayer (Horture hdl l) where
  initAudio = initHortureAudio
  deinitAudio = deinitProteaAudio
  clearAudio = clearProteaAudio
  playAudio = playHortureAudio
  withAudio = withHortureAudio

sampleFromFile' :: FilePath -> Float -> Horture l hdl Sample
sampleFromFile' fp = liftIO . sampleFromFile fp

withHortureAudio :: Horture hdl l a -> Horture hdl l ()
withHortureAudio action = do
  s <- get
  env <- ask
  let acquire = evalHorture s env initHortureAudio
      action' (_, s') = runHorture s' env action
      release (_, s') = runHorture s' env clearProteaAudio
  void . liftIO $ bracket acquire action' release

initHortureAudio :: Horture hdl l ()
initHortureAudio = do
  liftIO (runProteaPlayer def def initProteaAudio) >>= \case
    (Left _, _) -> throwError AudioSinkInitializationErr
    (Right _, _) -> return ()
  env <- asks (^. audioEnv)
  let files = Map.toList $ staticSoundFiles env
  soundSamplesFiles <- mapM (\(n, fp) -> (n,) <$> sampleFromFile' fp 0.6) files
  soundSamplesGenerated <- mapM (\(n, pcm) -> (n,) <$> generateSampleFromPCM pcm) [(FlashbangPeep, flashbangPeep)]
  modify $ \s -> s & audioState %~ \as -> as {staticSounds = Map.fromList $ soundSamplesFiles ++ soundSamplesGenerated}

playHortureAudio :: Sound StaticSoundEffect -> Horture l hdl ()
playHortureAudio a = do
  as <- gets (^. audioState)
  ae <- asks (^. audioEnv)
  (res, as') <- liftIO $ runProteaPlayer ae as (playProteaAudio a)
  case res of
    Left err -> throwError $ AudioSinkPlayErr err
    Right _ -> pure ()
  modify (\s -> s & audioState .~ as')
