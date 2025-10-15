module Horture.Logging
  ( logDummy,
    logChannel,
    withColog,
    logEvent',
    HortureLogger (..),
  )
where

import Colog
import Control.Concurrent.Chan.Synchronous
import Control.Monad.Reader
import Data.Text (Text, pack)
import Horture.Event
import Horture.Horture
import Horture.State
import Prelude hiding (log)

class (Monad m) => HortureLogger m where
  logEvent :: Event -> m ()
  logInfo :: Text -> m ()
  logError :: Text -> m ()
  logWarn :: Text -> m ()

-- TODO: Maybe add WriterT to Horture monad and use `tell` to embed the
-- Channel.
instance HortureLogger (Horture m 'Channel hdl) where
  logEvent ev = asks _logChan >>= (\la -> withColog Info la (logEvent' ev)) . logChannel
  logInfo msg = asks _logChan >>= (\la -> withColog Info la msg) . logChannel
  logError msg = asks _logChan >>= (\la -> withColog Error la msg) . logChannel
  logWarn msg = asks _logChan >>= (\la -> withColog Warning la msg) . logChannel

instance HortureLogger (Horture m 'NoLog hdl) where
  logEvent _ = return ()
  logInfo _ = return ()
  logError _ = return ()
  logWarn _ = return ()

logEvent' :: Event -> Text
logEvent' = pack . show

withColog :: (Monad m) => Severity -> LogAction m Text -> Text -> m ()
withColog s target = usingLoggerT (cmap fmtMessage target) . log s

-- | logDummy is a dummy log which does nothing.
logDummy :: (Monad m) => LogAction m t
logDummy = LogAction $ \_ -> return ()

-- | logChannel is a logging action writing its output to a channel handle.
logChannel :: (MonadIO m) => Maybe (Chan t) -> LogAction m t
logChannel (Just chan) = LogAction $ liftIO . asyncWriteChan chan
logChannel _ = logDummy
