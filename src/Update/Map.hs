module Update.Map where

import Data.Maybe

import Types.WorldMap

import Update.Player
import Update.Projectile
import Update.Mob

import Interactions

updateMap :: Float -> Float -> Map -> Map
updateMap dt localDt me = updateInteractions $ me {
  mapPlayer = updatePlayer dt localDt (mapPlayer me),
  mapProjectiles = map (updateProjectile dt localDt) (mapProjectiles me),
  mapMobs = mapMaybe (updateMob dt localDt) (mapMobs me)
}
