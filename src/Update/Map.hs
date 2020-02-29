module Update.Map where

import Types.WorldMap

import Update.Player
import Update.Projectile
import Interactions

updateMap :: Float -> Float -> Map -> Map
updateMap dt localDt me = updateInteractions $ me {
  mapPlayer = updatePlayer dt localDt (mapPlayer me),
  mapProjectiles = map (updateProjectile dt localDt) (mapProjectiles me)
}
