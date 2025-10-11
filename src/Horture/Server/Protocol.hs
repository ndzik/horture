{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Horture.Server.Protocol
  ( CCCommand (..),
    CCReply (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data CCCommand
  = CmdPing
  | CmdGrab -- pick/select window (you decide how)
  | CmdStopCapture
  | CmdToggleEvents
  | CmdEnableAll
  | CmdDisableAll
  | CmdRefresh
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
