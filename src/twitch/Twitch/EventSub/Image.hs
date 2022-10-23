module Twitch.EventSub.Image (Image (..)) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

data Image = Image
  { imageURL_1x :: !Text,
    imageURL_2x :: !Text,
    imageURL_4x :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("image_" :: String)) . camelTo2 '_'} ''Image)
