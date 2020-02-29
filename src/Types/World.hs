module Types.World where

import Types.WorldMap
import Types.Player

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

-- | Does the world's player have a specific item.
worldPlayerHasItem :: World -> String -> Bool
worldPlayerHasItem world = (`elem` (playerItems $ mapPlayer $ worldMap world))

-- | World timescale.
worldTimeScale :: World -> Float
worldTimeScale world
  | worldPlayerHasItem world "beatSlowdown" = 0.5
  | otherwise                               = 1
