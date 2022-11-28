module Horture.Loader.Error
  ( LoaderError (..),
  )
where

data LoaderError
  = LoaderDecodingGif !String
  | LoaderDecodingJPEG !String
  | LoaderDecodingPNG !String
  | LoaderUnsupportedAssetType !String
  | LoaderEmptyGif
  deriving (Show)
