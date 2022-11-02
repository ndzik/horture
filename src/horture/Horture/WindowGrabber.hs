{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Horture.WindowGrabber
  ( WindowGrabber (..),
    WindowPoller (..),
  )
where

-- | WindowGrabber allows accessing image data for a grabbed window.
class Monad m => WindowGrabber hdl m | m -> hdl where
  -- | grabAnyWindow grabs any window. It is implementation specific how this
  -- window is chosen. It could be via a user prompt, or some random area,
  -- application, frame.
  grabAnyWindow :: m hdl

class Monad m => WindowPoller hdl m | m -> hdl where
  -- | pollWindowEnvironment polls events which have to be handled by the
  -- application. This is necessary because most window managers are non-async
  -- designed, which requires constant polling.
  pollWindowEnvironment :: m ()

  -- | nextFrame gets the next frame of the captured window.
  nextFrame :: m ()
