module Horture.Events (pollHortureEvents) where

import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.STM (readTVarIO)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.RingBuffer as Ringbuffer
import Horture.Command
import Horture.Effect
import Horture.Error
import Horture.Event
import Horture.Horture
import Horture.Logging
import Horture.Scene
import Horture.State

pollHortureEvents :: (HortureLogger (Horture m l hdl)) => Float -> Float -> Scene -> Horture m l hdl (Maybe Scene)
pollHortureEvents timeNow dt s = do
  asks _eventChan
    >>= liftIO . tryReadChan
    >>= \case
      Success ev -> do
        logEvent ev
        handleHortureEvent timeNow dt ev s
      _otherwise -> do
        return (Just s)

handleHortureEvent :: Float -> Float -> Event -> Scene -> Horture m l hdl (Maybe Scene)
handleHortureEvent timeNow dt (EventEffect n eff) s = do
  rb <- asks (^. eventList) >>= liftIO . readTVarIO
  liftIO $ Ringbuffer.append (PastEvent timeNow n eff) rb
  Just <$> applyEffect timeNow dt s eff
handleHortureEvent _ _ (EventCommand Exit) _ = return Nothing
handleHortureEvent _ _ (EventCommand _cmd) _ = throwError $ HE "unimplemented"

applyEffect :: Float -> Float -> Scene -> Effect -> Horture m l hdl Scene
applyEffect timeNow dt s eff = return $ apply timeNow dt eff s
