{-# LANGUAGE CPP #-}

module Horture.Audio
  ( module R,
    module P,
#if defined(HORTURE_DARWIN)
    module MacOSAudio,
#endif
  )
where

import Horture.Audio.Player as P
import Horture.Audio.Recorder as R

#if defined(HORTURE_DARWIN)
import Horture.Audio.MacOS as MacOSAudio
#endif
