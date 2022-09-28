{-# LANGUAGE TypeApplications #-}

module Horture.X11
  ( imgPtr,
    szCInt,
    ximageData,
    ximagebitsPerPixel,
    ximageBytesPerLine,
    ximageOffset,
    ximageBitmapUnit,
    ximageBitmapPad,
    ximageRMask,
    ximageGMask,
    ximageBMask,
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

ximageRMask :: Image -> IO CULong
ximageRMask (Image p) = peekByteOff @CULong (castPtr p) xrmask
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xrmask = 4 * szCint + szPtr + 7 * szCint

ximageGMask :: Image -> IO CULong
ximageGMask (Image p) = peekByteOff @CULong (castPtr p) xgmask
  where
    szCint = sizeOf @CInt undefined
    szCULong = sizeOf @CULong undefined
    szPtr = sizeOf @CIntPtr undefined
    xgmask = 4 * szCint + szPtr + 7 * szCint + 1 * szCULong

ximageBMask :: Image -> IO CULong
ximageBMask (Image p) = peekByteOff @CULong (castPtr p) xgmask
  where
    szCint = sizeOf @CInt undefined
    szCULong = sizeOf @CULong undefined
    szPtr = sizeOf @CIntPtr undefined
    xgmask = 4 * szCint + szPtr + 7 * szCint + 2 * szCULong

ximageBitmapUnit :: Image -> IO CInt
ximageBitmapUnit (Image p) = peekByteOff @CInt (castPtr p) xbitmapUnit
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xbitmapUnit = 4 * szCint + szPtr + szCint

ximageBitmapPad :: Image -> IO CInt
ximageBitmapPad (Image p) = peekByteOff @CInt (castPtr p) xbitmapPad
  where
    szCint = sizeOf @CInt undefined
    szPtr = sizeOf @CIntPtr undefined
    xbitmapPad = 4 * szCint + szPtr + 3 * szCint

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
