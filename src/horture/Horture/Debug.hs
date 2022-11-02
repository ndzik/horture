-- | Horture.Debug contains auxiliary code useful for manual debugging.
module Horture.Debug
  ( dumpImageToFile,
  )
where

import Control.Monad
import Foreign (Storable (peekElemOff))
import Graphics.X11.Xlib (Image)
import Horture.X11
import System.IO (IOMode (AppendMode), hPrint, hPutStrLn, withFile)

dumpImageToFile :: FilePath -> Image -> Int -> IO ()
dumpImageToFile fp i h = do
  withFile fp AppendMode $ \hdl -> do
    src <- ximageData i
    bpp <- ximagebitsPerPixel i
    bpl <- ximageBytesPerLine i
    id <- ximageDepth i
    hPutStrLn hdl . unwords . map show $ [bpp, bpl, id]
    mapM_ (peekElemOff src >=> hPrint hdl) [0 .. (fromIntegral bpl * h)]
