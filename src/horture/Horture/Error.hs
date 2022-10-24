module Horture.Error (HortureError(..)) where

newtype HortureError = HE String deriving (Show)
