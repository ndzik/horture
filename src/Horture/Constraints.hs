{-# LANGUAGE ConstraintKinds #-}

module Horture.Constraints where

import Horture.Debug
import Horture.Horture
import Horture.Logging
import Horture.WindowGrabber

type HortureEffects m hdl l =
  ( HortureLogger (Horture m l hdl),
    WindowPoller hdl (Horture m l hdl),
    Debuggable (Horture m l hdl)
  )
