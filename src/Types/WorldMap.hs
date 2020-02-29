module Types.WorldMap where

import Data.Maybe
import Types.Player
import Types.FloorTile
import Types.Projectile

-- WorldMap {{{1

data Map = Map {
  mapFloorTiles :: [FloorTile],
  mapProjectiles :: [Projectile],
  mapPlayer :: Player
}

initMap :: Map
initMap = Map {
  mapFloorTiles = floorTiles,
  mapProjectiles = maybeToList $ initProjectileTowards (6, 0) (-1, 0),
  mapPlayer = initPlayer
}
