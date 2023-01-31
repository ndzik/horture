{-# LANGUAGE MultiParamTypeClasses #-}

module Horture.Audio.Player.Horture where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import Horture.Audio.Player.Player
import Horture.Audio.Player.Protea
import Horture.Horture
import Horture.Error
import Horture.State

instance AudioPlayer (Horture hdl l) where
  initAudio = initProteaAudio
  deinitAudio = deinitProteaAudio
  clearAudio = clearProteaAudio
  playAudio a = do
    as <- gets (^. audioState)
    ae <- asks (^. audioEnv)
    (res, as') <- liftIO $ runProteaPlayer ae as (playProteaAudio a)
    case res of
      Left _ -> throwError AudioSinkUnavailableErr
      Right _ -> pure ()
    modify (\s -> s & audioState .~ as')
