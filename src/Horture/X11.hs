{-# LANGUAGE TypeApplications #-}

module Horture.X11
  ( imgPtr,
    szCInt,
    ximageData,
    ximagebitsPerPixel,
    ximageBytesPerLine,
    ximageOffset,
    ximageDepth,
  )
where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Graphics.X11
import Graphics.X11.Xlib.Types

imgPtr :: Image -> Ptr Image
imgPtr (Image p) = p

szCInt :: Int
szCInt = sizeOf @CInt undefined

ximageData :: Image -> IO (Ptr Word8)
ximageData (Image p) = peek (plusPtr @Image @(Ptr CIntPtr) p xdataPtr) >>= \buf -> return . castPtr $ buf
  where
    szCint = sizeOf @CInt undefined
    xdataPtr = 4 * szCint

ximagebitsPerPixel :: Image -> IO CInt
ximagebitsPerPixel (Image p) = peekByteOff @CInt (castPtr p) xbitsPerPixel
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xbitsPerPixel = 4 * szCint + szPtr + 6 * szCint

ximageBytesPerLine :: Image -> IO CInt
ximageBytesPerLine (Image p) = peekByteOff @CInt (castPtr p) xbytesPerLine
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xbytesPerLine = 4 * szCint + szPtr + 5 * szCint

ximageDepth :: Image -> IO CInt
ximageDepth (Image p) = peekByteOff @CInt (castPtr p) xdepth
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xdepth = 4 * szCint + szPtr + 4 * szCint

ximageOffset :: Image -> IO CInt
ximageOffset (Image p) = peekByteOff @CInt (castPtr p) xoffset
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xwidth = 0
    xheight = szCint
    xoffset = 2 * szCint
    xformat = 3 * szCint
    xdataPtr = 4 * szCint
    xbyteorder = 4 * szCint + szPtr
    xbitmapUnit = 4 * szCint + szPtr + szCint
    xbitmapBitOrder = 4 * szCint + szPtr + 2 * szCint
    xbitmapPad = 4 * szCint + szPtr + 3 * szCint
    xdepth = 4 * szCint + szPtr + 4 * szCint
    xbytesPerLine = 4 * szCint + szPtr + 5 * szCint
    xbitsPerPixel = 4 * szCint + szPtr + 6 * szCint
