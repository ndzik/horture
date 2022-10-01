{-# LANGUAGE LambdaCase #-}

module Horture.Events (pollHortureEvents) where

import Control.Concurrent.Chan.Synchronous
import Control.Monad.Reader
import Horture.Effect
import Horture.Event
import Horture.Horture
import Horture.Scene
import Horture.State

pollHortureEvents :: Double -> Double -> Scene -> Horture Scene
pollHortureEvents timeNow dt s = do
  asks _eventChan >>= liftIO . tryReadChan
    >>= \case
      Success ev -> do
        handleHortureEvent timeNow dt s ev
      _otherwise -> do
        return s

handleHortureEvent :: Double -> Double -> Scene -> Event -> Horture Scene
handleHortureEvent timeNow dt s (EventEffect eff) = do
  applyEffect timeNow dt s eff
handleHortureEvent _ _ _ EventCommand = error "unimplemented"

applyEffect :: Double -> Double -> Scene -> Effect -> Horture Scene
applyEffect timeNow dt s eff@AddGif {} = do
  return $ apply timeNow dt eff s
applyEffect _ _ _ _ = error "unimplemented"
