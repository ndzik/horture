{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (newEmptyMVar)
import Control.Concurrent.Chan.Synchronous
import Data.Default
import Data.Text (Text)
import GHC.Conc (newTVarIO)
import Horture
import Horture.Backend as Backend
import Horture.Behaviour
import Horture.Event
import Horture.Horture (LoggingTarget (..))
import Horture.Initializer (HortureInitializerEnvironment (..), runHortureInitializer)
import Horture.Loader
import Horture.Object
import Horture.Render (indexForGif)
import Horture.Scene
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Horture"
    [ testGroup
        "Renderer"
        [ testCase "indexForGif" $ do
            let delays = [10, 2, 2, 23, 12]
                maxIndex = length delays - 1
            assertEqual "start_0" 0 $ indexForGif delays 0 maxIndex
            assertEqual "start_2" 0 $ indexForGif delays 2 maxIndex
            assertEqual "start_edge-2" 0 $ indexForGif delays 8 maxIndex
            assertEqual "start_edge-1" 0 $ indexForGif delays 9 maxIndex
            assertEqual "start_edge" 0 $ indexForGif delays 10 maxIndex
            assertEqual "mid_start_01" 1 $ indexForGif delays 11 maxIndex
            assertEqual "mid_edge_01" 1 $ indexForGif delays 12 maxIndex
            assertEqual "mid_start_02" 2 $ indexForGif delays 13 maxIndex
            assertEqual "mid_edge_02" 2 $ indexForGif delays 14 maxIndex
            assertEqual "mid_start_03" 3 $ indexForGif delays 15 maxIndex
            assertEqual "mid_edge_03" 3 $ indexForGif delays 37 maxIndex
            assertEqual "end_start" 4 $ indexForGif delays 38 maxIndex
            assertEqual "end_edge-1" 4 $ indexForGif delays 48 maxIndex
            assertEqual "end" 4 $ indexForGif delays 49 maxIndex
        ],
      testGroup "Backend.Grabber" $ [testBackendWindowSelection]
    ]

testBackendWindowSelection :: TestTree
testBackendWindowSelection = testCase "initializer" $ do
  logChan <- newChan @Text
  mv <- newEmptyMVar
  fc <- newTVarIO 0
  evChan <- newChan @Event
  let env =
        HortureInitializerEnvironment
          { _hortureInitializerEnvironmentLogChan = logChan,
            _hortureInitializerEnvironmentGrabbedWin = mv,
            _hortureInitializerEnvironmentDefaultFont = Nothing
          }
      -- startScene = def {_screen = def {_behaviours = [(pulse 0.5 2 1, 0, Forever)]}}
      startScene = def
      action = Backend.initialize @'NoLog startScene [] [] fc (Just logChan) evChan
  res <- runHortureInitializer env action
  print res
