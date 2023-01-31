{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Horture.Audio.Player.Protea where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Horture.Audio.Player.Effects
import Data.Default
import Horture.Audio.Player.Player
import Sound.ProteaAudio.SDL as Protea
  ( Sample,
    finishAudio,
    initAudio,
    sampleFromFile,
    sampleFromMemoryPcm,
    soundPlay,
    soundStopAll,
  )

newtype ProteaAudioPlayer a = ProteaAudioPlayer
  { unAudio :: ExceptT AudioPlayerError (StateT AudioPlayerState (ReaderT AudioPlayerEnv IO)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState AudioPlayerState,
      MonadReader AudioPlayerEnv,
      MonadIO,
      MonadError AudioPlayerError
    )

runProteaPlayer ::
  AudioPlayerEnv ->
  AudioPlayerState ->
  ProteaAudioPlayer a ->
  IO (Either AudioPlayerError a, AudioPlayerState)
runProteaPlayer env s = flip runReaderT env . flip runStateT s . runExceptT . unAudio

data AudioPlayerState = AudioPlayerState
  { generatedSounds :: !(Map.Map String Sample),
    dynamicSounds :: !(Map.Map FilePath Sample)
  }

instance Default AudioPlayerState where
  def = AudioPlayerState Map.empty Map.empty

data AudioPlayerError = StaticAudioSampleNotFoundErr deriving (Show)

newtype AudioPlayerEnv = AudioPlayerEnv
  { staticSounds :: Map.Map StaticSoundEffect Sample
  }

instance Default AudioPlayerEnv where
  def = AudioPlayerEnv Map.empty

instance AudioPlayer ProteaAudioPlayer where
  initAudio = initProteaAudio
  playAudio = playProteaAudio
  clearAudio = clearProteaAudio
  deinitAudio = liftIO finishAudio

initProteaAudio :: ( MonadIO m) => m Bool
initProteaAudio = liftIO $ Protea.initAudio 64 44100 1024

deinitProteaAudio :: ( MonadIO m) => m ()
deinitProteaAudio = liftIO finishAudio

clearProteaAudio :: (MonadIO m) => m ()
clearProteaAudio = liftIO soundStopAll

playProteaAudio ::
  ( MonadState AudioPlayerState m,
    MonadReader AudioPlayerEnv m,
    MonadError AudioPlayerError m,
    MonadIO m
  ) =>
  Sound StaticSoundEffect ->
  m ()
playProteaAudio (StaticSound FlashbangSFX) = do
  sample <-
    asks (Map.lookup FlashbangSFX . staticSounds) >>= \case
      Nothing -> throwError StaticAudioSampleNotFoundErr
      Just s -> return s
  void . liftIO $ soundPlay sample 1.0 1.0 0 1.0
playProteaAudio (GeneratedSound name pcm) = do
  sample <-
    gets (Map.lookup name . generatedSounds) >>= \case
      Nothing -> generateSample name pcm
      Just s -> return s
  void . liftIO $ soundPlay sample 1.0 1.0 0 1.0
playProteaAudio (DynamicSound fp) = do
  sample <-
    gets (Map.lookup fp . dynamicSounds) >>= \case
      Nothing -> generateFileSample fp
      Just s -> return s
  void . liftIO $ soundPlay sample 1.0 1.0 0 1.0

generateFileSample ::
  ( MonadState AudioPlayerState m,
    MonadReader AudioPlayerEnv m,
    MonadIO m
  ) =>
  FilePath ->
  m Sample
generateFileSample fp = do
  sample <- liftIO (sampleFromFile fp 1.0)
  modify (\pas -> pas {dynamicSounds = Map.insert fp sample (dynamicSounds pas)})
  return sample

generateSample ::
  ( MonadState AudioPlayerState m,
    MonadReader AudioPlayerEnv m,
    MonadIO m
  ) =>
  String ->
  PCM ->
  m Sample
generateSample name (PCM bs chans sampleRate bitsPerSample) = do
  sample <- liftIO (sampleFromMemoryPcm bs chans sampleRate bitsPerSample 1.0)
  modify (\pas -> pas {generatedSounds = Map.insert name sample (generatedSounds pas)})
  return sample
