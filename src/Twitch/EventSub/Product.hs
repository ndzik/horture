module Twitch.EventSub.Product (Product (..)) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

data Product = Product
  { productName :: !Text
  , productBits :: !Int
  , productSku :: !Text
  , productInDevelopment :: !Bool
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length ("product_" :: String)) . camelTo2 '_'} ''Product)
