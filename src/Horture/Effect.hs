module Horture.Effect
  ( Effect (..),
  GifIndex,
  Position,
    mkGifEffects,
  )
where

import qualified Data.Map.Strict as Map
import Horture.Gif
import Horture.Object
import Linear.V3

type GifIndex = FilePath

type Position = V3 Float

data Effect
  = AddGif !GifIndex !Lifetime !Position
  | ShakeIt
  | ZoomIt
  | FlipIt
  | Rollercoaster
  | BlazeIt
  | Flashbang
  | Noop
  deriving (Show, Eq)

-- | mkGifEffects creates effect constructors from the given HortureGIF cache.
mkGifEffects :: Map.Map FilePath HortureGIF -> [Lifetime -> Position -> Effect]
mkGifEffects = Map.foldl (\effs hg -> AddGif (_gifFullPath hg) : effs) []
