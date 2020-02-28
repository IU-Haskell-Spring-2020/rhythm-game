module Types.FloorTile where

import Data.List

-- FloorTile {{{1

data FloorTile = FloorTile {
  floorTilePosition :: (Float, Float)
}

initFloorTile :: (Float, Float) -> FloorTile
initFloorTile pos = FloorTile { floorTilePosition = pos }


-- Map initializers {{{1

floorTiles :: [FloorTile]
floorTiles = map initFloorTile coords
  where
    coords = vertical `union` horizontal
    vertical = [(x, y) | x <- [0..3], y <- [0..6]]
    horizontal = [(x, y) | x <- [0..6], y <- [0..3]]