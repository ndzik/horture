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
import System.FilePath.Posix

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
  deriving (Eq)

instance Show Effect where
  show (AddGif fp lt pos) = unwords ["AddGif", takeFileName fp, show lt, show pos]
  show ShakeIt = "ShakeIt"
  show ZoomIt = "ZoomIt"
  show FlipIt = "FlipIt"
  show Rollercoaster = "Rollercoaster"
  show BlazeIt = "BlazeIt"
  show Flashbang = "Flashbang"
  show Noop = "Noop"

-- | mkGifEffects creates effect constructors from the given HortureGIF cache.
mkGifEffects :: Map.Map FilePath HortureGIF -> [Lifetime -> Position -> Effect]
mkGifEffects = Map.foldr (\hg effs -> AddGif (_gifFullPath hg) : effs) []
