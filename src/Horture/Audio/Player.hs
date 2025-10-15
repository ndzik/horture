{-# LANGUAGE CPP #-}
module Horture.Audio.Player
  (
#if defined(PROTEA_ENABLED) && defined(HORTURE__LINUX)
  module O,
#endif
    module P,
    module E,
  )
where

import Horture.Audio.Player.Player as P
import Horture.Audio.Player.Effects as E

#if defined(PROTEA_ENABLED) && defined(HORTURE_LINUX)
import Horture.Audio.Player.Protea as O
#endif
