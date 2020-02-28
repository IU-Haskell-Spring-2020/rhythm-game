module Types.World where

import Types.WorldMap

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