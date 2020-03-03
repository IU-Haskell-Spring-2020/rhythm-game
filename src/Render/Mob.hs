module Render.Mob where

import Framework.Types
import Grid
import Types.SmoothPosition
import Types.Mob

renderMob :: Mob -> GridPicture
renderMob me = scaled scale $ translated pos $ texture "mob"
  where
    scale = 0.8
    pos = mobDisplayPosition me
    name
      | mobShouldDie me = "mob_red"
      | otherwise       = "mob"
