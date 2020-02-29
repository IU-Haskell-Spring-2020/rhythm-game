module Types.Projectile where

import Types.SmoothPosition
import Math

data ProjectileDirection = Up | Down | Left | Right | None deriving Eq

data Projectile = Projectile {
  projectilePosition :: SmoothPosition,
  projectileDirection :: ProjectileDirection,
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
        | x < 0 && y == 0 = Just Types.Projectile.Left
        | x > 0 && y == 0 = Just Types.Projectile.Right
        | otherwise       = Nothing

      replaceDirection direction = (initDefaultProjectile position) {
        projectileDirection = direction
      }
