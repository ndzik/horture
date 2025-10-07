module Horture.Backend.Types
  ( PixelFmt (..),
    WindowId (..),
    Source (..),
    Frame (..),
    Capture (..),
  )
where

import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr)

data PixelFmt = BGRA8Premult | RGBA8
  deriving (Show, Eq)

newtype WindowId = WindowId Word64
  deriving (Show, Eq)

data Source
  = SourceWindow WindowId
  | SourceDisplay Word32
  deriving (Show, Eq)

data Frame = Frame
  { width :: !Int,
    height :: !Int,
    stride :: !Int, -- bytes per row
    fmt :: !PixelFmt,
    ptr :: !(Ptr Word8) -- raw pixel memory
  }

data Capture = Capture
  { listWindows :: IO [(WindowId, Text)],
    startStream :: Source -> (Frame -> IO ()) -> IO (IO ()) -- returns a stop action
  }
