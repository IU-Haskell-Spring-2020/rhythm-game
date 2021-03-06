module Update.Projectile where

import Data.Fixed

import Math

import Types.Projectile
import Types.SmoothPosition
import Types.Direction

import Update.SmoothPosition

projectileDirectionVector :: Projectile -> V2
projectileDirectionVector me = directionToVector (projectileDirection me)

updateProjectile :: Float -> Float -> Projectile -> Projectile
updateProjectile dt localDt me = moveProjectile localDt $ me {
  projectilePosition = updateSmoothPosition (projectilePosition me) dt
}

moveProjectile :: Float -> Projectile -> Projectile
moveProjectile localDt me
  | projectileMoved me && dt > 0.5 = me { projectileMoved = False }
  | not (projectileMoved me) && dt < 0.5 = me {
    projectileMoved = True,
    projectilePosition = (projectilePosition me) {
      smoothPositionPrevious = smoothPositionCurrent (projectilePosition me),
      smoothPositionCurrent
        = vAdd
            (smoothPositionCurrent (projectilePosition me))
            (projectileDirectionVector me),
      smoothPositionTime = 1
    }
  }
  | otherwise = me
    where
      dt = localDt `mod'` 0.25 * 4
