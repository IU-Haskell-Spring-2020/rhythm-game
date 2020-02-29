module Types.Projectile where

import Types.SmoothPosition
import Types.Direction

import Math


data Projectile = Projectile {
  projectilePosition :: SmoothPosition,
  projectileDirection :: Direction,
  projectileMoved :: Bool
}

initDefaultProjectile :: (Float, Float) -> Projectile
initDefaultProjectile position = Projectile {
  projectilePosition = initSmoothPositionAt position,
  projectileDirection = None,
  projectileMoved = False
}

initProjectileTowards :: (Float, Float) -> V2 -> Maybe Projectile
initProjectileTowards position towards
  = fmap replaceDirection (extractDirection towards)
    where
      extractDirection (x, y)
        | x == 0 && y > 0 = Just Down
        | x == 0 && y < 0 = Just Up
        | x < 0 && y == 0 = Just Types.Direction.Left
        | x > 0 && y == 0 = Just Types.Direction.Right
        | otherwise       = Nothing

      replaceDirection direction = (initDefaultProjectile position) {
        projectileDirection = direction
      }
