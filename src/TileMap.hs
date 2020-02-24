module TileMap where

import Data.List
import qualified FloorTile

floorTiles :: [FloorTile.FloorTile]
floorTiles = map FloorTile.init coords
  where
    coords = vertical `union` horizontal
    vertical = [(x, y) | x <- [0..3], y <- [0..6]]
    horizontal = [(x, y) | x <- [0..6], y <- [0..3]]

