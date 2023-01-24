module Horture.Effect
  ( Effect (..),
    ShaderEffect (..),
    AssetIndex,
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

type AssetIndex = FilePath

type Position = V3 Float

data Effect
  = AddAsset !AssetIndex !Lifetime !Position ![Behaviour]
  | AddScreenBehaviour !Lifetime ![Behaviour]
  | AddShaderEffect !Lifetime !ShaderEffect
  | AddRapidFire ![Effect]
  | Noop

effectToCost :: Effect -> Int
effectToCost AddAsset {} = 1
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourAudiophile _]) = 4
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourShake _]) = 3
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourRotate _]) = 4
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourMoveTo _]) = 5
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourCircle _]) = 5
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourPulse _]) = 3
effectToCost (AddScreenBehaviour _ _) = 2
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
  | BassRealityWarp
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Show Effect where
  show (AddAsset fp lt pos _) = unwords ["AddImage", takeFileName fp, show lt, show pos]
  show (AddScreenBehaviour _ _) = "AddScreenBehaviour"
  show (AddShaderEffect lt eff) = unwords ["AddShaderEffect", show lt, show eff]
  show (AddRapidFire effs) = unwords ("AddRapidFire" : map show effs)
  show Noop = "Noop"

class Entitled d where
  toTitle :: d -> Text

instance Entitled Effect where
  toTitle (AddAsset "" _ _ _) = "RandomGifOrImage"
  toTitle (AddAsset n _ _ _) = pack . takeFileName $ n
  toTitle (AddScreenBehaviour _ [behaviour]) = toTitle behaviour
  toTitle (AddScreenBehaviour _ _) = "RandomScreenEffect"
  toTitle (AddShaderEffect _ eff) = toTitle eff
  toTitle (AddRapidFire _) = "RATATATATA"
  toTitle Noop = "Nothing"

instance Entitled Behaviour where
  toTitle (Behaviour name _) = toTitle name

instance Entitled BehaviourType where
  toTitle BehaviourAudiophile = "hearAndFeelIt"
  toTitle BehaviourShake = "shakeIt"
  toTitle BehaviourRotate = "rotateIt"
  toTitle BehaviourMoveTo = "moveIt"
  toTitle BehaviourCircle = "circleIt"
  toTitle BehaviourPulse = "pulseIt"

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
  toTitle BassRealityWarp = "BassRealityWarp"

class FromText d where
  fromText :: Text -> d

instance FromText Effect where
  fromText "AddImage" = AddAsset "" (Limited 8) (V3 0 0 0) []
  fromText "AddScreenBehaviour" = AddScreenBehaviour (Limited 8) []
  fromText "AddRapidFire" = AddRapidFire []
  fromText _ = Noop
