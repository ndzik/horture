module Horture.Path (resolvePath) where

import System.Directory (getHomeDirectory)
import Data.Functor ((<&>))

resolvePath :: FilePath -> IO FilePath
resolvePath ('~':rs) = getHomeDirectory <&> (++ rs)
resolvePath rs = return rs
