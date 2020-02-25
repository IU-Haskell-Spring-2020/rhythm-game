module Types where

import Data.List
import Framework.Types
import Math

-- World {{{1

data World = World {
  worldPassedTime :: Float,
  worldMap :: Map
}

-- | World state at the beginning.
initWorld :: World
initWorld = World {
  worldPassedTime = 0,
  worldMap = initMap
}

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

-- FloorTile {{{1

data FloorTile = FloorTile {
  floorTilePosition :: (Float, Float)
}

initFloorTile :: (Float, Float) -> FloorTile
initFloorTile pos = FloorTile { floorTilePosition = pos }

-- Player {{{1

data Player = Player {
  playerLastPosition :: (Float, Float),
  playerPosition :: (Float, Float),
  playerAnimationTime :: Float,

  playerCanMove :: Bool,
  playerMoved :: Bool,
  playerErrorTime :: Float
}

initPlayer :: Player
initPlayer = Player {
  playerLastPosition = (0, 0),
  playerPosition = (0, 0),
  playerAnimationTime = 0,
  playerCanMove = False,
  playerMoved = False,
  playerErrorTime = 0
}

playerCurrentPosition :: Player -> (Float, Float)
playerCurrentPosition me = pos
  where
    transitionTime = min 1 (sqrt ((1 - playerAnimationTime me) * 5))
    dPos = vMul (vSub (playerPosition me) (playerLastPosition me)) transitionTime
    pos = vAdd (playerLastPosition me) dPos

-- Map initializers {{{1

floorTiles :: [FloorTile]
floorTiles = map initFloorTile coords
  where
    coords = vertical `union` horizontal
    vertical = [(x, y) | x <- [0..3], y <- [0..6]]
    horizontal = [(x, y) | x <- [0..6], y <- [0..3]]

-- vim: set fdm=marker:
