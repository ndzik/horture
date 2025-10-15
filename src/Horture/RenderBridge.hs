{-# LANGUAGE ForeignFunctionInterface #-}

module Horture.RenderBridge
  ( RB,
    RBFrame (..),
    c_rb_create,
    c_rb_destroy,
    c_rb_start,
    c_rb_stop,
    c_rb_poll,
    c_rb_release,
    c_rb_upload,
    rbPoll,
    rbPeekWidth,
    rbPeekHeight,
  )
where

import Foreign
import Foreign.C
import Graphics.GL (GLsizei)

data RB

data RBFrame = RBFrame
  { kind :: CInt,
    w :: CInt,
    h :: CInt,
    stride :: CInt,
    payload :: Ptr (),
    handle :: Ptr ()
  }

instance Storable RBFrame where
  sizeOf _ = 6 * ptrSize where ptrSize = sizeOf (nullPtr :: Ptr ())
  alignment _ = alignment (undefined :: CInt)
  peek _ = error "peek not needed"
  poke p (RBFrame k w h s pl hd) = do
    pokeByteOff p 0 k
    pokeByteOff p 4 w
    pokeByteOff p 8 h
    pokeByteOff p 12 s
    pokeByteOff p 16 pl
    pokeByteOff p (16 + ptrSize) hd
    where
      ptrSize = sizeOf (nullPtr :: Ptr ())

foreign import ccall safe "rbridge.h rb_create" c_rb_create :: IO (Ptr RB)

foreign import ccall safe "rbridge.h rb_destroy" c_rb_destroy :: Ptr RB -> IO ()

foreign import ccall safe "rbridge.h rb_start_capture" c_rb_start :: CULong -> Ptr RB -> CString -> CSize -> IO CInt

foreign import ccall safe "rbridge.h rb_stop_capture" c_rb_stop :: Ptr RB -> IO ()

foreign import ccall safe "rbridge.h rb_poll_frame" c_rb_poll :: Ptr RB -> Ptr RBFrame -> IO CInt

foreign import ccall safe "rbridge.h rb_release_frame" c_rb_release :: Ptr RB -> Ptr RBFrame -> IO ()

foreign import ccall unsafe "rbridge.h rb_upload_to_bound_texture" c_rb_upload :: Ptr RB -> Ptr RBFrame -> IO CInt

rbPoll :: Ptr RB -> IO (Maybe RBFrame)
rbPoll rb = alloca $ \pf -> do
  rc <- c_rb_poll rb pf
  if rc == 1 then Just <$> peek pf else pure Nothing

rbPeekWidth :: Ptr RBFrame -> IO GLsizei
rbPeekWidth p = peekByteOff p 4

rbPeekHeight :: Ptr RBFrame -> IO GLsizei
rbPeekHeight p = peekByteOff p 8
