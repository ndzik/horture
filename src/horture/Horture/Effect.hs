module Horture.Effect
  ( Effect (..),
    ShaderEffect (..),
    AssetIndex,
    Position,
    FromText (..),
    Entitled (..),
    effectToCost,
    effectToImage,
    effectToColor,
  )
where

import Data.Text (Text, pack)
import Horture.Audio.Player
import Horture.Object
import Linear.V3
import System.FilePath.Posix
import Twitch.EventSub

type AssetIndex = FilePath

type Position = V3 Float

data Effect
  = AddAsset !AssetIndex !Lifetime !Position ![Behaviour]
  | AddScreenBehaviour !Lifetime ![Behaviour]
  | AddShaderEffect !Lifetime !ShaderEffect ![Sound StaticSoundEffect]
  | RemoveScreenBehaviour !Int
  | RemoveShaderEffect !Int
  | AddRapidFire ![Effect]
  | Noop

effectToCost :: Effect -> Int
effectToCost AddAsset {} = 1
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourAudiophile _]) = 4
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourShake _]) = 3
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourRotate _]) = 6
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourMoveTo _]) = 5
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourCircle _]) = 6
effectToCost (AddScreenBehaviour _ [Behaviour BehaviourPulse _]) = 3
effectToCost (AddScreenBehaviour _ _) = 2
effectToCost (AddShaderEffect _ Barrel _) = 4
effectToCost (AddShaderEffect _ Blur _) = 3
effectToCost (AddShaderEffect _ Stitch _) = 6
effectToCost (AddShaderEffect _ Flashbang _) = 3
effectToCost (AddShaderEffect _ Cycle _) = 6
effectToCost (AddShaderEffect _ Blink _) = 2
effectToCost (AddShaderEffect _ Mirror _) = 5
effectToCost (AddShaderEffect _ Invert _) = 3
effectToCost (AddShaderEffect _ Toonify _) = 3
effectToCost (AddShaderEffect _ Audiophile _) = 4
effectToCost (AddShaderEffect _ BassRealityWarp _) = 3
effectToCost (RemoveScreenBehaviour _) = 3
effectToCost (RemoveShaderEffect _) = 2
effectToCost AddRapidFire {} = 6
effectToCost Noop {} = 0

effectToColor :: Effect -> Text
effectToColor AddAsset {} = "#66ffff"
effectToColor (AddScreenBehaviour _ [Behaviour BehaviourAudiophile _]) = "#ff66ff"
effectToColor (AddScreenBehaviour _ [Behaviour BehaviourShake _]) = "#660066"
effectToColor (AddScreenBehaviour _ [Behaviour BehaviourRotate _]) = "#660066"
effectToColor (AddScreenBehaviour _ [Behaviour BehaviourMoveTo _]) = "#660066"
effectToColor (AddScreenBehaviour _ [Behaviour BehaviourCircle _]) = "#660066"
effectToColor (AddScreenBehaviour _ [Behaviour BehaviourPulse _]) = "#660066"
effectToColor (AddScreenBehaviour _ _) = "#660066"
effectToColor (AddShaderEffect _ Barrel _) = "#ffff66"
effectToColor (AddShaderEffect _ Blur _) = "#669999"
effectToColor (AddShaderEffect _ Stitch _) = "#663300"
effectToColor (AddShaderEffect _ Flashbang _) = "#ffffff"
effectToColor (AddShaderEffect _ Cycle _) = "#ccffcc"
effectToColor (AddShaderEffect _ Blink _) = "#000000"
effectToColor (AddShaderEffect _ Mirror _) = "#ffcc99"
effectToColor (AddShaderEffect _ Invert _) = "#66ffff"
effectToColor (AddShaderEffect _ Toonify _) = "#99cc00"
effectToColor (AddShaderEffect _ Audiophile _) = "#cc0000"
effectToColor (AddShaderEffect _ BassRealityWarp _) = "#7a7a52"
effectToColor (RemoveScreenBehaviour _) = "#ff0000"
effectToColor (RemoveShaderEffect _) = "#ff0000"
effectToColor AddRapidFire {} = "#006600"
effectToColor Noop {} = "#000000"

effectToImage :: Effect -> Maybe Image
effectToImage AddAsset {} = Nothing
effectToImage (AddScreenBehaviour _ [Behaviour BehaviourAudiophile _]) = Nothing
effectToImage (AddScreenBehaviour _ [Behaviour BehaviourShake _]) = Nothing
effectToImage (AddScreenBehaviour _ [Behaviour BehaviourRotate _]) = Nothing
effectToImage (AddScreenBehaviour _ [Behaviour BehaviourMoveTo _]) = Nothing
effectToImage (AddScreenBehaviour _ [Behaviour BehaviourCircle _]) = Nothing
effectToImage (AddScreenBehaviour _ [Behaviour BehaviourPulse _]) = Nothing
effectToImage (AddScreenBehaviour _ _) = Nothing
effectToImage (AddShaderEffect _ Barrel _) = Nothing
effectToImage (AddShaderEffect _ Blur _) = Nothing
effectToImage (AddShaderEffect _ Stitch _) = Nothing
effectToImage (AddShaderEffect _ Flashbang _) = Nothing
effectToImage (AddShaderEffect _ Cycle _) = Nothing
effectToImage (AddShaderEffect _ Blink _) = Nothing
effectToImage (AddShaderEffect _ Mirror _) = Nothing
effectToImage (AddShaderEffect _ Invert _) = Nothing
effectToImage (AddShaderEffect _ Toonify _) = Nothing
effectToImage (AddShaderEffect _ Audiophile _) = Nothing
effectToImage (AddShaderEffect _ BassRealityWarp _) = Nothing
effectToImage (RemoveScreenBehaviour _) = Nothing
effectToImage (RemoveShaderEffect _) = Nothing
effectToImage AddRapidFire {} = Nothing
effectToImage Noop {} = Nothing

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
  show (AddShaderEffect lt eff _) = unwords ["AddShaderEffect", show lt, show eff]
  show (AddRapidFire effs) = unwords ("AddRapidFire" : map show effs)
  show (RemoveScreenBehaviour _) = "RemoveScreenBehaviour"
  show (RemoveShaderEffect _) = "RemoveShaderEffect"
  show Noop = "Noop"

class Entitled d where
  toTitle :: d -> Text

instance Entitled Effect where
  toTitle (AddAsset "" _ _ _) = "RandomGifOrImage"
  toTitle (AddAsset n _ _ _) = pack . takeFileName $ n
  toTitle (AddScreenBehaviour _ [behaviour]) = toTitle behaviour
  toTitle (AddScreenBehaviour _ _) = "RandomScreenEffect"
  toTitle (AddShaderEffect _ eff _) = toTitle eff
  toTitle (AddRapidFire _) = "RATATATATA"
  toTitle (RemoveScreenBehaviour _) = "PurgeEffect"
  toTitle (RemoveShaderEffect _) = "PurgeShader"
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
  fromText "AddShaderEffect" = AddShaderEffect Forever Barrel []
  fromText "AddRapidFire" = AddRapidFire []
  fromText "RemoveShaderEffect" = RemoveShaderEffect 0
  fromText "RemoveScreenBehaviour" = RemoveScreenBehaviour 0
  fromText "ThiccIt"=  AddShaderEffect Forever Barrel []
  fromText "WhereAreMyGlasses?"=  AddShaderEffect Forever Blur []
  fromText "GrandmaSaysHi"=  AddShaderEffect Forever Stitch []
  fromText "FLASHBANG"=  AddShaderEffect Forever Flashbang []
  fromText "TakeTheWhitePill"=  AddShaderEffect Forever Cycle []
  fromText "EyesClosed"=  AddShaderEffect Forever Blink []
  fromText "Discombobulated"=  AddShaderEffect Forever Mirror []
  fromText "InvertColors"=  AddShaderEffect Forever Invert []
  fromText "Toonify"=  AddShaderEffect Forever Toonify []
  fromText "Audiophile"=  AddShaderEffect Forever Audiophile []
  fromText "BassRealityWarp"=  AddShaderEffect Forever BassRealityWarp []
  fromText "hearAndFeelIt"=  AddScreenBehaviour (Limited 8) [Behaviour BehaviourAudiophile (\_ _ o -> o)]
  fromText "shakeIt"=  AddScreenBehaviour (Limited 8) [Behaviour BehaviourShake (\_ _ o -> o)]
  fromText "rotateIt"=  AddScreenBehaviour (Limited 8) [Behaviour BehaviourRotate (\_ _ o -> o)]
  fromText "moveIt"=  AddScreenBehaviour (Limited 8) [Behaviour BehaviourMoveTo (\_ _ o -> o)]
  fromText "circleIt"=  AddScreenBehaviour (Limited 8) [Behaviour BehaviourCircle (\_ _ o -> o)]
  fromText "pulseIt"=  AddScreenBehaviour (Limited 8) [Behaviour BehaviourPulse (\_ _ o -> o)]
  fromText "RandomGifOrImage" = AddAsset "" Forever (V3 0 0 0) []
  fromText "RandomScreenEffect" = AddScreenBehaviour Forever []
  fromText "RATATATATA" = AddRapidFire []
  fromText "PurgeEffect" = RemoveScreenBehaviour 0
  fromText "PurgeShader" = RemoveShaderEffect 0
  fromText _ = Noop
