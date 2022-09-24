module Horture.Effect (Effect (..), GifType (..)) where

data GifType = Ricardo
  deriving (Show)

data Effect
  = AddGif GifType
  | ShakeIt
  | ZoomIt
  | FlipIt
  | Rollercoaster
  | BlazeIt
  | Flashbang
  | Noop
  deriving (Show)
