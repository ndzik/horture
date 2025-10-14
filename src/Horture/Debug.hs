-- | Horture.Debug contains auxiliary code useful for manual debugging.
module Horture.Debug
  ( Debuggable (..),
  )
where

import Data.Text (Text)

class Debuggable m where
  timeCPU :: Text -> m a -> m a
