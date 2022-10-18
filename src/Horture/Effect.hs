module Horture.Effect
  ( Effect (..),
    GifIndex,
    Position,
    mkGifEffects,
    FromText (..),
  )
where

import Control.Lens
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Horture.Gif
import Horture.Object
import Linear.V3
import System.FilePath.Posix

type GifIndex = FilePath

type Position = V3 Float

data Effect
  = AddGif !GifIndex !Lifetime !Position ![Behaviour]
  | ShakeIt
  | ZoomIt
  | FlipIt
  | Rollercoaster
  | BlazeIt
  | Flashbang
  | Noop

instance Show Effect where
  show (AddGif fp lt pos _) = unwords ["AddGif", takeFileName fp, show lt, show pos]
  show ShakeIt = "ShakeIt"
  show ZoomIt = "ZoomIt"
  show FlipIt = "FlipIt"
  show Rollercoaster = "Rollercoaster"
  show BlazeIt = "BlazeIt"
  show Flashbang = "Flashbang"
  show Noop = "Noop"

class FromText d where
  fromText :: Text -> d

instance FromText Effect where
  fromText "ShakeIt" = ShakeIt
  fromText "ZoomIt" = ZoomIt
  fromText "FlipIt" = FlipIt
  fromText "Rollercoaster" = Rollercoaster
  fromText "BlazeIt" = BlazeIt
  fromText "Flashbang" = Flashbang
  fromText "AddGif" = AddGif "" (Limited 8) (V3 0 0 0) []
  fromText _= Noop

-- | mkGifEffects creates effect constructors from the given HortureGIF cache.
mkGifEffects :: Map.Map FilePath HortureGif -> [Lifetime -> Position -> [Behaviour]-> Effect]
mkGifEffects = Map.foldr (\hg effs -> AddGif (hg ^. fullPath) : effs) []
