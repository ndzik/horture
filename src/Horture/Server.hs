{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Horture.Server
  ( ServerCfg (..),
    startServer,
    serverAlive,
    sendStop,
    spawnServerProcess,
    waitUntilUp,
    hortureExePath,
    interactWithHorture,
    tryConnectServer,
  )
where

import Control.Concurrent (MVar, forkIO, killThread, myThreadId, newEmptyMVar, readMVar, threadDelay, throwTo)
import Control.Concurrent.Chan.Synchronous (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception (SomeException, finally, try)
import Control.Lens
import Control.Monad (forever, void)
import Data.Aeson
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Horture.Audio.Player.Player (Sound (GeneratedSound), flashbangPeep)
import qualified Horture.Backend as Backend
import Horture.Behaviour
import Horture.Effect (Effect (AddScreenBehaviour, AddShaderEffect), ShaderEffect (Flashbang))
import Horture.Event
import Horture.EventSource.Local (hortureLocalEventSource)
import Horture.Horture
import Horture.Initializer
import Horture.Loader
import Horture.Loader.Asset (Asset)
import Horture.Object
import Horture.Scene (Scene (..))
import Horture.Server.Env
import Horture.Server.Protocol
import Linear (V3 (V3))
import Network.WebSockets (Connection)
import qualified Network.WebSockets as WS
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import UnliftIO (catchAny)
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
  evChan <- newChan @Event
  let imagesDir = "./assets/images/"
  preloadedImages <-
    runPreloader (PLC $ imagesDir) loadAssetsInMemory >>= \case
      Left _ -> pure []
      Right pli -> pure pli
  void . forkIO $ runServer fpsTvar evChan writerPumpChan hortureTID logChan mv esEnabled
  _run fpsTvar evChan logChan mv esEnabled preloadedImages
  where
    runServer fpsTvar evChan writerPumpChan tid logChan mv enabledTvar = do
      let env =
            WSAppEnv
              { _wsAppEnvLogChan = logChan,
                _wsAppEnvWriterPumpChan = writerPumpChan,
                _wsAppEnvMV = mv,
                _wsAppEnvEnabledTVar = enabledTvar,
                _wsAppEnvFPSTVar = fpsTvar,
                _wsAppEnvEvChan = evChan,
                _wsAppEnvHortureTID = tid
              }
      WS.runServer host port $ wsApp env

wsApp :: WSAppEnv -> WS.PendingConnection -> IO ()
wsApp env pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 25 (pure ()) $ do
    (`catchAny` report "wsApp-ping") $ do
      workerTIDS <- newTVarIO []
      let clientEnv =
            WSClientEnv
              { _wsAppEnvWorkerTIDs = workerTIDS,
                _wsAppEnvConn = conn
              }
      catchAny (mainLoop clientEnv) (report "mainLoop")
        `finally` do
          workerTIDs <- readTVarIO (clientEnv ^. wsAppEnvWorkerTIDs)
          mapM_ killThread workerTIDs
          print @Text "Connection closed, all threads killed"
  where
    pumpLoop :: Chan CCReply -> Connection -> IO ()
    pumpLoop writeChan conn = forever $ do
      -- Wait for mvar to be filled and send CaptureWindow message
      mWindow <- readMVar (env ^. wsAppEnvMV)
      WS.sendTextData conn (encode (RCapturedWindow mWindow))
      forever $ do
        msg <- readChan writeChan
        WS.sendTextData conn (encode msg)
    logLoop :: Chan Text -> Chan CCReply -> IO ()
    logLoop logChan writeChan = forever $ do
      msg <- readChan logChan
      enqueue writeChan . ROut $ msg
    mainLoop :: WSClientEnv -> IO ()
    mainLoop clientEnv = do
      let conn = clientEnv ^. wsAppEnvConn
      raw <- WS.receiveData conn
      case eitherDecode raw of
        Left _ -> WS.sendTextData conn (encode (RErr "bad json"))
        Right CmdPing -> WS.sendTextData conn (encode RPong)
        Right CmdStopCapture -> do
          -- Shutdown server.
          WS.sendTextData conn (encode ROk)
          WS.sendClose conn (T.pack "stopping")
          print @Text "Shutting down server as requested"
          throwTo (env ^. wsAppEnvHortureTID) ExitSuccess
        Right CmdStartCapture -> do
          logTID <-
            forkIO $
              catchAny
                (logLoop (env ^. wsAppEnvLogChan) (env ^. wsAppEnvWriterPumpChan))
                (report "logLoop-thread")
          fpsTID <-
            forkIO $
              catchAny
                (fpsTicker (env ^. wsAppEnvFPSTVar) conn)
                (report "fpsTicker-thread")
          writerTID <-
            forkIO $
              catchAny
                (pumpLoop (env ^. wsAppEnvWriterPumpChan) conn)
                (report "pumpLoop-thread")
          atomically . modifyTVar (clientEnv ^. wsAppEnvWorkerTIDs) $ (writerTID :) . (fpsTID :) . (logTID :)
          WS.sendTextData conn (encode ROk)
        Right CmdToggleEvents -> do
          atomically . modifyTVar (env ^. wsAppEnvEnabledTVar) $ not
          evStreamState <- readTVarIO (env ^. wsAppEnvEnabledTVar)
          writeChan (env ^. wsAppEnvLogChan) $ ("Toggled event stream -> " <> (T.pack . show $ evStreamState))
        Right (CmdTriggerEffect eff) -> triggerEffect eff (env ^. wsAppEnvEvChan)
        Right _ -> WS.sendTextData conn (encode ROk)
      mainLoop clientEnv
    report :: Text -> SomeException -> IO ()
    report where_ e = print $ "Exception in " <> where_ <> ": " <> T.pack (show e)

triggerEffect :: EffectRequest -> Chan Event -> IO ()
triggerEffect (ERShader eff lt) evChan = do
  case eff of
    Flashbang -> writeChan evChan . EventEffect "CC" $ AddShaderEffect lt eff [GeneratedSound "flashbangPeep" flashbangPeep]
    _ -> writeChan evChan . EventEffect "CC" $ AddShaderEffect lt eff []
triggerEffect (ERBehaviour bt lt) evChan = do
  let behaviourList = case bt of
        BehaviourAudiophile -> [audiophile]
        BehaviourShake -> [shake 10 2 10]
        BehaviourRotate -> [rotate 10]
        BehaviourMoveTo -> [moveTo (V3 0 0 (-5))]
        BehaviourCircle -> [circle 2 10]
        BehaviourPulse -> [pulse 10 2 10]
        BehaviourBob -> [bob 2 10]
        BehaviourBounce -> [bounce 2 10]
        BehaviourSway -> [sway 0.5 10]
        BehaviourWobble -> [wobble 1.5 10 10]
        BehaviourSpiralTo -> [spiralTo (V3 1 1 (-5)) 5 10]
        BehaviourOrbit -> [orbit (V3 (-4) (-3) (-4)) 10 10]
        BehaviourBreathe -> [breathe 10 10]
        BehaviourJitter -> [jitter 10 10]
        BehaviourEaseTo -> [easeTo (V3 (-2) 2 (-5)) 0.5]
        BehaviourFlipX -> [flipXPulse 10]
  writeChan evChan . EventEffect "CC" $ AddScreenBehaviour lt behaviourList

fpsTicker :: TVar Int -> Connection -> IO ()
fpsTicker fpsTVar conn = go 0
  where
    go lastFrameCount = do
      threadDelay $ 1 * 1000 * 1000 -- 1 second
      currentFrameCount <- readTVarIO fpsTVar
      let fps = currentFrameCount - lastFrameCount
      WS.sendTextData conn (encode (RFPS $ fromIntegral fps))
      go currentFrameCount

_run :: TVar Int -> Chan Event -> Chan Text -> MVar Text -> TVar Bool -> [(FilePath, Asset)] -> IO ()
_run fpsTvar evChan logChan mv enabledTVar preloadedImages = do
  let timeout = round $ (1.0 * 1000.0 * 1000.0 :: Double)
      images = map fst preloadedImages
  esTID <- forkIO $ hortureLocalEventSource timeout evChan images enabledTVar
  let defaultFont = Just "./assets/fonts/CaskaydiaMonoNerdFontMono-Regular.ttf"
  let env =
        HortureInitializerEnvironment
          { _hortureInitializerEnvironmentLogChan = logChan,
            _hortureInitializerEnvironmentGrabbedWin = mv,
            _hortureInitializerEnvironmentDefaultFont = defaultFont
          }
      startScene =
        def
          { _shaders = [],
            _screen =
              def
                { _behaviours = []
                }
          }
      action = Backend.initialize @'Channel startScene preloadedImages [] fpsTvar (Just logChan) evChan
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

tryConnectServer :: Int -> (CCReply -> IO ()) -> Chan CCCommand -> IO Bool
tryConnectServer 0 _ _ = pure False
tryConnectServer n action chan = do
  r :: Either SomeException () <- try $ connectServer action chan
  case r of
    Left err -> do
      putStrLn $ "Failed to connect to server: " ++ show err
      threadDelay 500000
      tryConnectServer (n - 1) action chan
    Right _ -> pure True

connectServer :: (CCReply -> IO ()) -> Chan CCCommand -> IO ()
connectServer action chan = do
  WS.runClient (scHost def) (scPort def) "/" $ \conn -> do
    _ <- forkIO $ forever $ do
      cmd <- readChan chan
      WS.sendTextData conn (encode cmd)
    forever $ do
      msg <- WS.receiveData conn
      case eitherDecode msg of
        Left err -> putStrLn $ "Failed to decode message: " ++ err
        Right reply -> action reply

sendStop :: IO Bool
sendStop = do
  r :: Either SomeException Bool <- try $ WS.runClient (scHost def) (scPort def) "/" $ \c -> do
    WS.sendTextData c (encode CmdStopCapture)
    msg <- WS.receiveData c
    case eitherDecode msg of
      Right ROk -> pure True
      _ -> pure False
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
      if alive
        then pure True
        else threadDelay 200000 >> go (n - 1)

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
