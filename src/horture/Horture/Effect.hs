module Horture.Effect
  ( Effect (..),
    ShaderEffect (..),
    GifIndex,
    Position,
    FromText (..),
    Entitled (..),
    effectToCost,
  )
where

import Data.Text (Text, pack)
import Horture.Object
import Linear.V3
import System.FilePath.Posix

type GifIndex = FilePath

type Position = V3 Float

data Effect
  = AddGif !GifIndex !Lifetime !Position ![Behaviour]
  | AddScreenBehaviour !Lifetime ![Behaviour]
  | AddShaderEffect !Lifetime !ShaderEffect
  | AddRapidFire ![Effect]
  | Noop

effectToCost :: Effect -> Int
effectToCost AddGif {} = 1
effectToCost AddScreenBehaviour {} = 2
effectToCost AddShaderEffect {} = 4
effectToCost AddRapidFire {} = 6
effectToCost Noop {} = 0

data ShaderEffect
  = Barrel
  | Blur
  | Stitch
  | Flashbang
  | Cycle
  | Blink
  | Mirror
  | Invert
  | Toonify
  | Audiophile
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Show Effect where
  show (AddGif fp lt pos _) = unwords ["AddGif", takeFileName fp, show lt, show pos]
  show (AddScreenBehaviour _ _) = "AddScreenBehaviour"
  show (AddShaderEffect lt eff) = unwords ["AddShaderEffect", show lt, show eff]
  show (AddRapidFire effs) = unwords ("AddRapidFire" : map show effs)
  show Noop = "Noop"

class Entitled d where
  toTitle :: d -> Text

instance Entitled Effect where
  toTitle (AddGif n _ _ _) = pack . takeFileName $ n
  toTitle (AddScreenBehaviour _ _) = "RandomScreenEffect"
  toTitle (AddShaderEffect _ eff) = toTitle eff
  toTitle (AddRapidFire _) = "RATATATATA"
  toTitle Noop = "Nothing"

instance Entitled ShaderEffect where
  toTitle Barrel = "ThiccIt"
  toTitle Blur = "WhereAreMyGlasses?"
  toTitle Stitch = "GrandmaSaysHi"
  toTitle Flashbang = "FLASHBANG"
  toTitle Cycle = "TakeTheWhitePill"
  toTitle Blink = "EyesClosed"
  toTitle Mirror = "Discombobulated"
  toTitle Invert = "InvertColors"
  toTitle Toonify = "Toonify"
  toTitle Audiophile = "Audiophile"

class FromText d where
  fromText :: Text -> d

instance FromText Effect where
  fromText "AddGif" = AddGif "" (Limited 8) (V3 0 0 0) []
  fromText "AddScreenBehaviour" = AddScreenBehaviour (Limited 8) []
  fromText "AddRapidFire" = AddRapidFire []
  fromText _ = Noop
