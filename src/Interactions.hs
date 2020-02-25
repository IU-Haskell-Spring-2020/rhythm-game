module Interactions where

import Types

updateInteractions :: Map -> Map
updateInteractions me = me {
  mapPlayer = updatePlayerInteractions me
}

updatePlayerInteractions :: Map -> Player
updatePlayerInteractions map = if shouldSwitch then newPlayer else currentPlayer
  where
    currentPlayer = mapPlayer map
    newPlayer = currentPlayer {
      playerPosition = playerLastPosition currentPlayer,
      playerLastPosition = playerPosition currentPlayer
    }

    currentPlayerPosition = playerPosition currentPlayer

    -- TODO: this code is really full of magic numbers
    shouldSwitch = colliding && (playerAnimationTime currentPlayer < 0.95)

    -- | Write first, optimize later :)
    colliding = not (any isTileSameAsPlayer $ mapFloorTiles map)
    isTileSameAsPlayer tile = floorTilePosition tile == currentPlayerPosition
