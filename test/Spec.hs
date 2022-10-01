module Main where

import Horture.Loader
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Loader"
    [ testGroup
        "isGif"
        [ testCase "noGif" $
            isGif "noGif" @?= False,
          testCase "noGif with extension" $
            isGif "noGif.jpeg" @?= False,
          testCase "isGif" $
            isGif "isGif.gif" @?= True
        ]
    ]
