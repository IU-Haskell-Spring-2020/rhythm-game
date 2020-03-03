module Types.WorldMap where

import Data.Maybe
import Types.Player
import Types.Mob
import Types.FloorTile
import Types.Projectile
import Types.Items

-- WorldMap {{{1

data Map = Map {
  mapFloorTiles :: [FloorTile],
  mapProjectiles :: [Projectile],
  mapItemTiles :: [ItemTile],
  mapMobs :: [Mob],
  mapPlayer :: Player
}

initMap :: Map
initMap = Map {
  mapFloorTiles = floorTiles,
  -- mapProjectiles = maybeToList $ initProjectileTowards (6, 0) (-1, 0),
  mapProjectiles = [],
  mapItemTiles = itemTiles,
  mapMobs = [initDefaultMob (5, 1)],
  mapPlayer = initPlayer
}
