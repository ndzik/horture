module Horture.Effect (Effect (..), GifType (..)) where

import Horture.Object
import Linear.V3

data GifType = Ricardo
  deriving (Show)

type Position = V3 Float

data Effect
  = AddGif GifType Lifetime Position
  | ShakeIt
  | ZoomIt
  | FlipIt
  | Rollercoaster
  | BlazeIt
  | Flashbang
  | Noop
  deriving (Show)
