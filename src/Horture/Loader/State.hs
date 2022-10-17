{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Horture.Loader.State where

import Control.Lens.TH
import Data.Default
import Data.Map
import Horture.Gif

-- | LoaderState describes the state of the horture loader.
newtype LoaderState = LS
  { -- | Gifs which were already resolved and loaded as textures.
    _loaderStateResolvedGifs :: Map FilePath HortureGif
  }
  deriving (Show)

instance Default LoaderState where
  def =
    LS
      { _loaderStateResolvedGifs = empty
      }

makeFields ''LoaderState
