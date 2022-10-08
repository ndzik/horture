module Twitch.EventSub.Product (Product (..)) where

import Data.Text (Text)

data Product = Product
  { productName :: !Text
  , productBits :: !Int
  , productSku :: !Text
  , productInDevelopment :: !Bool
  }
  deriving (Show)
