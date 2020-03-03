module Update.Map where

import Data.Maybe

import Framework.Types
import Types.WorldMap

import Update.Player
import Update.Projectile
import Update.Mob

import Interactions

updateMap :: Float -> Float -> Map -> (Map, Action)
updateMap dt localDt me = updateInteractions $ me {
  mapPlayer = updatePlayer dt localDt (mapPlayer me),
  mapProjectiles = map (updateProjectile dt localDt) (mapProjectiles me),
  mapMobs = mapMaybe (updateMob dt localDt) (mapMobs me)
}
