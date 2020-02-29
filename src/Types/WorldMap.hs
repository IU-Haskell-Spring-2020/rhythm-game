module Types.WorldMap where

import Types.Player
import Types.FloorTile
import Types.Items

-- WorldMap {{{1

data Map = Map {
  mapFloorTiles :: [FloorTile],
  mapItemTiles :: [ItemTile],
  mapPlayer :: Player
}

initMap :: Map
initMap = Map {
  mapFloorTiles = floorTiles,
  mapItemTiles = itemTiles,
  mapPlayer = initPlayer
}
