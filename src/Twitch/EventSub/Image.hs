module Twitch.EventSub.Image (Image(..)) where

import Data.Text (Text)

data Image = Image { imageURL1x :: !Text
                   , imageURL2x :: !Text
                   , imageURL4x :: !Text
                   } deriving Show
