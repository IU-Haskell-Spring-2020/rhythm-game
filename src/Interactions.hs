module Interactions where

import Types.WorldMap
import Types.Player
import Types.SmoothPosition
import Types.FloorTile

updateInteractions :: Map -> Map
updateInteractions me = me {
  mapPlayer = updatePlayerInteractions me
}

updatePlayerInteractions :: Map -> Player
updatePlayerInteractions map = if shouldSwitch then newPlayer else currentPlayer
  where
    currentPlayer = mapPlayer map
    playerSmoothPosition = playerPosition currentPlayer
    playerSmoothPositionNew = playerSmoothPosition {
      smoothPositionCurrent = smoothPositionPrevious playerSmoothPosition,
      smoothPositionPrevious = smoothPositionCurrent playerSmoothPosition
    }
    newPlayer = currentPlayer {
      playerPosition = playerSmoothPositionNew
    }

    currentPlayerPosition = playerPosition currentPlayer

    -- TODO: this code is really full of magic numbers
    shouldSwitch = colliding && (smoothPositionTime (playerPosition currentPlayer) < 0.95)

    -- | Write first, optimize later :)
    colliding = not (any isTileSameAsPlayer $ mapFloorTiles map)
    isTileSameAsPlayer tile = floorTilePosition tile == smoothPositionCurrent currentPlayerPosition
