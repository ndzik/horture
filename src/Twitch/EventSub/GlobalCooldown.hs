module Twitch.EventSub.GlobalCooldown (GlobalCooldown(..)) where

data GlobalCooldown = GlobalCooldown { globalcooldownIsEnabled :: !Bool
                                     , globalcooldownSeconds :: !Int
                                     } deriving Show
