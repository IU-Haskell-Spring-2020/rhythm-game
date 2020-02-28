module Types.WorldMap where

import Types.Player
import Types.FloorTile

-- WorldMap {{{1

data Map = Map {
  mapFloorTiles :: [FloorTile],
  mapPlayer :: Player
}

initMap :: Map
initMap = Map {
  mapFloorTiles = floorTiles,
  mapPlayer = initPlayer
}