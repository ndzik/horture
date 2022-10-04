module Main (main) where

import Horture.CommandCenter.CommandCenter

-- | I want the following:
-- Start the command center which gives an overview over available assets
-- (GIFs), Metainformation which window is being captured, if any and show
-- logging output from the underlying renderer.
--
-- Create a connection to twitch API to read Horture-Requests which get applied
-- to the active scene OR cached.
--
-- When a window is hortured, then pass available assets to the loader, s.t. we
-- have a single source of thruth.
main :: IO ()
main = runCommandCenter
