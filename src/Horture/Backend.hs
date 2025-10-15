{-# LANGUAGE CPP #-}

module Horture.Backend
  ( module B,
  )
where

#if defined(HORTURE_LINUX)
import Horture.Backend.X11 as B
#endif

#if defined(HORTURE_DARWIN)
import Horture.Backend.MacOS as B
#endif
