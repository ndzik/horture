{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Horture.EventSource.Logger
  ( Logger (..),
    logInfo,
    logError,
    logWarn,
    runColog,
    runHortureChannelLogger,
  )
where

import Colog (Severity (..), cmap, fmtMessage, log, logTextStdout, usingLoggerT)
import Control.Concurrent.Chan.Synchronous
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Data.Text (Text, pack)
import Horture.CommandCenter.Event
import Prelude hiding (log)

data Logger l where
  LogInfo :: Text -> Logger ()
  LogError :: Text -> Logger ()
  LogWarn :: Text -> Logger ()

makeEffect ''Logger

runColog :: (LastMember IO effs) => Eff (Logger : effs) a -> Eff effs a
runColog = interpretM $ \case
  LogInfo msg -> withColog Info msg
  LogError msg -> withColog Error msg
  LogWarn msg -> withColog Warning msg

withColog :: Severity -> Text -> IO ()
withColog s = usingLoggerT (cmap fmtMessage logTextStdout) . log s

runHortureChannelLogger :: (LastMember IO effs) => Chan CommandCenterEvent -> Eff (Logger : effs) a -> Eff effs a
runHortureChannelLogger chan = interpretM $ \case
  LogInfo msg -> writeChan chan . CCLog . pack . show $ (Info, msg)
  LogError msg -> writeChan chan . CCLog . pack . show $ (Error, msg)
  LogWarn msg -> writeChan chan . CCLog . pack . show $ (Warning, msg)
