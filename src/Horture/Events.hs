module Horture.Events (pollHortureEvents) where

import Control.Concurrent.Chan.Synchronous
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Horture.Command
import Horture.Effect
import Horture.Error
import Horture.Event
import Horture.Horture
import Horture.Logging
import Horture.Scene
import Horture.State
import qualified RingBuffers.Lifted as Ringbuffer

pollHortureEvents :: (HortureLogger (Horture l hdl)) => Double -> Double -> Scene -> Horture l hdl (Maybe Scene)
pollHortureEvents timeNow dt s = do
  asks _eventChan >>= liftIO . tryReadChan
    >>= \case
      Success ev -> do
        logEvent ev
        handleHortureEvent timeNow dt ev s
      _otherwise -> do
        return (Just s)

handleHortureEvent :: Double -> Double -> Event -> Scene -> Horture l hdl (Maybe Scene)
handleHortureEvent timeNow dt (EventEffect n eff) s = do
  gets (^. eventList) >>= liftIO . Ringbuffer.append (PastEvent timeNow n eff)
  Just <$> applyEffect timeNow dt s eff
handleHortureEvent _ _ (EventCommand Exit) _ = return Nothing
handleHortureEvent _ _ (EventCommand _cmd) _ = throwError $ HE "unimplemented"

applyEffect :: Double -> Double -> Scene -> Effect -> Horture l hdl Scene
applyEffect timeNow dt s eff = return $ apply timeNow dt eff s
