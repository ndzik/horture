{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Horture.Constraints where

import Horture.Horture
import Horture.Logging
import Horture.WindowGrabber

type HortureEffects hdl l =
  ( HortureLogger (Horture l hdl),
    WindowPoller hdl (Horture l hdl)
  )
