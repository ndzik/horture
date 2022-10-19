module Horture.Effect
  ( Effect (..),
    GifIndex,
    Position,
    FromText (..),
  )
where

import Data.Text (Text)
import Horture.Object
import Linear.V3
import System.FilePath.Posix

type GifIndex = FilePath

type Position = V3 Float

data Effect
  = AddGif !GifIndex !Lifetime !Position ![Behaviour]
  | AddScreenBehaviour !Lifetime ![Behaviour]
  | BlazeIt
  | Flashbang
  | Noop

instance Show Effect where
  show (AddGif fp lt pos _) = unwords ["AddGif", takeFileName fp, show lt, show pos]
  show (AddScreenBehaviour _ _) = "AddScreenBehaviour"
  show BlazeIt = "BlazeIt"
  show Flashbang = "Flashbang"
  show Noop = "Noop"

class FromText d where
  fromText :: Text -> d

instance FromText Effect where
  fromText "BlazeIt" = BlazeIt
  fromText "Flashbang" = Flashbang
  fromText "AddGif" = AddGif "" (Limited 8) (V3 0 0 0) []
  fromText "AddScreenBehaviour" = AddScreenBehaviour (Limited 8) []
  fromText _ = Noop
