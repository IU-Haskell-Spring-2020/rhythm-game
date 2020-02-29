module Types.WorldMap where

import Data.Maybe
import Types.Player
import Types.FloorTile
import Types.Projectile
import Types.Items

-- WorldMap {{{1

data Map = Map {
  mapFloorTiles :: [FloorTile],
  mapProjectiles :: [Projectile],
  mapItemTiles :: [ItemTile],
  mapPlayer :: Player
}

initMap :: Map
initMap = Map {
  mapFloorTiles = floorTiles,
  mapProjectiles = maybeToList $ initProjectileTowards (6, 0) (-1, 0),
  mapItemTiles = itemTiles,
  mapPlayer = initPlayer
}
