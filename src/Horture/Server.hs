{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Horture.Server
  ( ServerCfg (..),
    startServer,
    serverAlive,
    sendStop,
    spawnServerProcess,
    waitUntilUp,
    hortureExePath,
    interactWithHorture,
  )
where

import Control.Concurrent (MVar, forkIO, killThread, modifyMVar_, myThreadId, newEmptyMVar, newMVar, readMVar, threadDelay, throwTo)
import Control.Concurrent.Chan.Synchronous (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception (SomeException, finally, try)
import Control.Monad (forever, void)
import Data.Aeson
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import qualified Horture.Backend as Backend
import Horture.Behaviour
import Horture.Effect (ShaderEffect (..))
import Horture.Event
import Horture.EventSource.Local (hortureLocalEventSource)
import Horture.Horture
import Horture.Initializer
import Horture.Object (Lifetime (..), Object (_behaviours))
import Horture.Scene (Scene (..))
import Horture.Server.Protocol
import Linear (V3 (V3))
import Network.WebSockets (Connection)
import qualified Network.WebSockets as WS
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import UnliftIO.Process (CreateProcess (..), ProcessHandle, createProcess, proc)

data ServerCfg = ServerCfg
  { scHost :: String,
    scPort :: Int
  }

instance Default ServerCfg where
  def = ServerCfg "127.0.0.1" 9198

enqueue :: Chan CCReply -> CCReply -> IO ()
enqueue chan msg = writeChan chan msg

startServer :: ServerCfg -> IO ()
startServer (ServerCfg host port) = do
  hortureTID <- myThreadId
  logChan <- newChan @Text
  writerPumpChan <- newChan @CCReply
  mv <- newEmptyMVar
  esEnabled <- newTVarIO False
  fpsTvar <- newTVarIO 0
  void . forkIO $ runServer fpsTvar writerPumpChan hortureTID logChan mv esEnabled
  _run fpsTvar logChan mv esEnabled
  where
    runServer fpsTvar writerPumpChan tid logChan mv enabledTvar = do
      running <- newMVar True
      WS.runServer host port $ \pending -> do
        conn <- WS.acceptRequest pending
        WS.withPingThread conn 25 (pure ()) $ do
          logTID <- forkIO . forever $ readChan logChan >>= enqueue writerPumpChan . ROut
          fpsTID <- forkIO $ fpsTicker fpsTvar conn
          writerTID <- forkIO $ do
            -- Wait for mvar to be filled and send CaptureWindow message
            mWindow <- readMVar mv
            WS.sendTextData conn (encode (RCapturedWindow mWindow))
            forever $ do
              msg <- readChan writerPumpChan
              WS.sendTextData conn (encode msg)
          loop tid running conn enabledTvar logChan
            `finally` mapM_ killThread [writerTID, logTID, fpsTID]
    loop tid running conn enabledTvar logChan = do
      raw <- WS.receiveData conn
      case eitherDecode raw of
        Left _ -> WS.sendTextData conn (encode (RErr "bad json"))
        Right CmdPing -> WS.sendTextData conn (encode RPong)
        Right CmdStopCapture -> do
          -- Shutdown server.
          WS.sendTextData conn (encode ROk)
          modifyMVar_ running (\_ -> pure False)
          WS.sendClose conn (T.pack "stopping")
          print @Text "Shutting down server as requested"
          throwTo tid ExitSuccess
        Right CmdToggleEvents -> do
          atomically . modifyTVar enabledTvar $ not
          evStreamState <- readTVarIO enabledTvar
          writeChan logChan $ ("Toggled event stream -> " <> (T.pack . show $ evStreamState))
        Right _ -> WS.sendTextData conn (encode ROk)

      loop tid running conn enabledTvar logChan

fpsTicker :: TVar Int -> Connection -> IO ()
fpsTicker fpsTVar conn = go 0
  where
    go lastFrameCount = do
      threadDelay $ 1 * 1000 * 1000 -- 1 second
      currentFrameCount <- readTVarIO fpsTVar
      let fps = currentFrameCount - lastFrameCount
      WS.sendTextData conn (encode (RFPS $ fromIntegral fps))
      go currentFrameCount

_run :: TVar Int -> Chan Text -> MVar Text -> TVar Bool -> IO ()
_run fpsTvar logChan mv enabledTVar = do
  evChan <- newChan @Event
  let timeout = round $ 0.2 * 1000.0 * 1000.0
      images = []
  esTID <- forkIO $ hortureLocalEventSource timeout evChan images enabledTVar
  let env =
        HortureInitializerEnvironment
          { _hortureInitializerEnvironmentLogChan = logChan,
            _hortureInitializerEnvironmentGrabbedWin = mv,
            _hortureInitializerEnvironmentDefaultFont = Nothing
          }
      startScene =
        def
          { _shaders = [],
            _screen =
              def
                { _behaviours = []
                }
          }
      action = Backend.initialize @'Channel startScene [] [] fpsTvar (Just logChan) evChan
  res <- runHortureInitializer env action
  killThread esTID
  print res

hortureExePath :: IO FilePath
hortureExePath = getExecutablePath

serverAlive :: IO Bool
serverAlive = do
  -- Just try to open a WS connection and see if it works
  r :: Either SomeException Bool <- try $ WS.runClient (scHost def) (scPort def) "/" $ \_c -> do
    pure True
  print ("serverAlive result: " ++ show r)
  pure (either (const False) id r)

sendStop :: IO Bool
sendStop = do
  r :: Either SomeException Bool <- try $ WS.runClient (scHost def) (scPort def) "/" $ \c -> do
    print @Text "Connected to capture server, sending stop command"
    WS.sendTextData c (encode CmdStopCapture)
    print @Text "Sent stop command, waiting for reply"
    msg <- WS.receiveData c
    print ("Received reply: " ++ show msg)
    case eitherDecode msg of
      Right ROk -> pure True
      _ -> pure False
  print ("sendStop result: " ++ show r)
  pure (either (const False) id r)

spawnServerProcess :: FilePath -> [String] -> IO ProcessHandle
spawnServerProcess exe args = do
  (_, _, _, ph) <- createProcess $ (proc exe args) {create_group = True}
  pure ph

waitUntilUp :: Int -> IO Bool
waitUntilUp retries = go retries
  where
    go 0 = pure False
    go n = do
      alive <- serverAlive
      if alive then pure True else threadDelay 200000 >> go (n - 1)

interactWithHorture :: (CCReply -> IO ()) -> Chan CCCommand -> IO ()
interactWithHorture onReply cmdChan = do
  WS.runClient (scHost def) (scPort def) "/" $ \conn -> do
    _ <- forkIO $ forever $ do
      cmd <- readChan cmdChan
      WS.sendTextData conn (encode cmd)
    forever $ do
      msg <- WS.receiveData conn
      case eitherDecode msg of
        Left err -> putStrLn $ "Failed to decode message: " ++ err
        Right reply -> onReply reply
