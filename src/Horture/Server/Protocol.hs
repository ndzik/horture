{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Horture.Server.Protocol
  ( CCCommand (..),
    CCReply (..),
    EffectRequest (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Horture.Effect
import Horture.Object

data EffectRequest
  = ERShader ShaderEffect Lifetime
  | ERBehaviour BehaviourType Lifetime
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data CCCommand
  = CmdPing
  | CmdGrab -- pick/select window (you decide how)
  | CmdStopCapture
  | CmdStartCapture
  | CmdToggleEvents
  | CmdEnableAll
  | CmdDisableAll
  | CmdRefresh
  | CmdTriggerEffect EffectRequest
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data CCReply
  = RPong
  | ROk
  | RCapturedWindow {windowTitle :: Text}
  | RErr {msg :: Text}
  | ROut {line :: Text}
  | RStatus {captured :: Bool, title :: Maybe Text}
  | RFPS {fps :: Double}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
