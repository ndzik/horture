module Horture.CommandCenter.Event (CommandCenterEvent (..)) where

import Data.Text (Text)

newtype CommandCenterEvent = CCLog Text deriving (Show)
