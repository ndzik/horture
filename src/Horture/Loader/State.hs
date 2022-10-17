module Horture.Loader.State
  ( LoaderState (..),
  )
where

import Data.Default
import Data.Map
import Horture.Gif

-- | LoaderState describes the state of the horture loader.
newtype LoaderState = LS
  { -- | Gifs which were already resolved and loaded as textures.
    _resolvedGifs :: Map FilePath HortureGIF
  }
  deriving (Show)

instance Default LoaderState where
  def =
    LS
      { _resolvedGifs = empty
      }
