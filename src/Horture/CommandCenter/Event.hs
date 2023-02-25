module Horture.CommandCenter.Event (CommandCenterEvent (..)) where

import Data.Text (Text)

data CommandCenterEvent = CCLog !Text
  | CCFrameUpdate !Float
  deriving (Show)
