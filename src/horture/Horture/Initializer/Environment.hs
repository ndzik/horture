{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Horture.Initializer.Environment
  ( HortureInitializerEnvironment (..),
    logChan,
    grabbedWin,
  )
where

import Control.Concurrent.Chan.Synchronous
import Control.Concurrent.MVar
import Control.Lens
import Data.Text

data HortureInitializerEnvironment = HortureInitializerEnvironment
  { _hortureInitializerEnvironmentLogChan :: !(Chan Text),
    _hortureInitializerEnvironmentGrabbedWin :: !(MVar (Maybe String))
  }

makeFields ''HortureInitializerEnvironment
