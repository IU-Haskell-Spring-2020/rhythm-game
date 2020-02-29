module Render.Projectile where

import Framework.Types
import Grid
import Types.Direction
import Types.Projectile
import Types.SmoothPosition

renderProjectile :: Projectile -> GridPicture
renderProjectile me = translated currentPosition $ texture name
  where
    currentPosition = currentSmoothPosition (projectilePosition me)
    name
      | projectileDirection me == Types.Direction.Left  = "projectile_left"
      | projectileDirection me == Types.Direction.Right = "projectile_right"
      | projectileDirection me == Up                     = "projectile_up"
      | projectileDirection me == Down                   = "projectile_down"
      | otherwise                                        = "blank"
