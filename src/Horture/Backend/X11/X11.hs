module Horture.Backend.X11.X11
  ( imgPtr,
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
    updateXWindowTexture,
  )
where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL as GL
import Graphics.X11
import Graphics.X11.Xlib.Types

imgPtr :: Image -> Ptr Image
imgPtr (Image p) = p

szCint :: Int
szCint = sizeOf @CInt undefined

szPtr :: Int
szPtr = sizeOf @CIntPtr undefined

szCULong :: Int
szCULong = sizeOf @CULong undefined

ximageData :: Image -> IO (Ptr Word8)
ximageData (Image p) = peek (plusPtr @Image @(Ptr CIntPtr) p xdataPtr) >>= \buf -> return . castPtr $ buf
  where
    xdataPtr = 4 * szCint

ximagebitsPerPixel :: Image -> IO CInt
ximagebitsPerPixel (Image p) = peekByteOff @CInt (castPtr p) xbitsPerPixel
  where
    xbitsPerPixel = 4 * szCint + szPtr + 6 * szCint

ximageBytesPerLine :: Image -> IO CInt
ximageBytesPerLine (Image p) = peekByteOff @CInt (castPtr p) xbytesPerLine
  where
    xbytesPerLine = 4 * szCint + szPtr + 5 * szCint

ximageDepth :: Image -> IO CInt
ximageDepth (Image p) = peekByteOff @CInt (castPtr p) xdepth
  where
    xdepth = 4 * szCint + szPtr + 4 * szCint

ximageRMask :: Image -> IO CULong
ximageRMask (Image p) = peekByteOff @CULong (castPtr p) xrmask
  where
    xrmask = 4 * szCint + szPtr + 7 * szCint

ximageGMask :: Image -> IO CULong
ximageGMask (Image p) = peekByteOff @CULong (castPtr p) xgmask
  where
    xgmask = 4 * szCint + szPtr + 7 * szCint + 1 * szCULong

ximageBMask :: Image -> IO CULong
ximageBMask (Image p) = peekByteOff @CULong (castPtr p) xgmask
  where
    xgmask = 4 * szCint + szPtr + 7 * szCint + 2 * szCULong

ximageBitmapUnit :: Image -> IO CInt
ximageBitmapUnit (Image p) = peekByteOff @CInt (castPtr p) xbitmapUnit
  where
    xbitmapUnit = 4 * szCint + szPtr + szCint

ximageBitmapPad :: Image -> IO CInt
ximageBitmapPad (Image p) = peekByteOff @CInt (castPtr p) xbitmapPad
  where
    xbitmapPad = 4 * szCint + szPtr + 3 * szCint

ximageOffset :: Image -> IO CInt
ximageOffset (Image p) = peekByteOff @CInt (castPtr p) xoffset
  where
    szCint = sizeOf @CInt undefined
    xoffset = 2 * szCint

-- | updateWindowTexture updates the OpenGL texture for the captured window
-- using the given dimensions together with the source image as a data source.
updateXWindowTexture :: (Int, Int) -> Image -> IO ()
updateXWindowTexture (w, h) i = do
  src <- ximageData i
  let pd = PixelData BGRA UnsignedInt8888Rev src
  texSubImage2D
    Texture2D
    0
    (TexturePosition2D 0 0)
    (TextureSize2D (fromIntegral w) (fromIntegral h))
    pd
  destroyImage i
