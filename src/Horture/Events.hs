{-# LANGUAGE LambdaCase #-}

module Horture.Events (pollHortureEvents) where

import Control.Concurrent.Chan.Synchronous
import Control.Monad.Reader
import Horture.Command
import Horture.Effect
import Horture.Event
import Horture.Horture
import Horture.Scene
import Horture.State

pollHortureEvents :: Double -> Double -> Scene -> Horture (Maybe Scene)
pollHortureEvents timeNow dt s = do
  asks _eventChan >>= liftIO . tryReadChan
    >>= \case
      Success ev -> do
        handleHortureEvent timeNow dt s ev
      _otherwise -> do
        return (Just s)

handleHortureEvent :: Double -> Double -> Scene -> Event -> Horture (Maybe Scene)
handleHortureEvent timeNow dt s (EventEffect eff) = Just <$> applyEffect timeNow dt s eff
handleHortureEvent _ _ _ (EventCommand Exit) = return Nothing
handleHortureEvent _ _ _ (EventCommand _cmd) = error "unimplemented"

applyEffect :: Double -> Double -> Scene -> Effect -> Horture Scene
applyEffect timeNow dt s eff@AddGif {} = do
  return $ apply timeNow dt eff s
applyEffect _ _ _ _ = error "unimplemented"
