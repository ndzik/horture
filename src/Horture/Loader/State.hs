module Horture.Loader.State where

import Control.Lens.TH
import Data.Default
import Data.Map
import Horture.Asset

-- | LoaderState describes the state of the horture loader.
newtype LoaderState = LS
  { -- | Gifs which were already resolved and loaded as textures.
    _loaderStateResolvedGifs :: Map FilePath HortureAsset
  }
  deriving (Show)

instance Default LoaderState where
  def =
    LS
      { _loaderStateResolvedGifs = empty
      }

makeFields ''LoaderState
