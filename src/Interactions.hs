module Interactions where

import Data.List
import Data.Maybe

import Types.WorldMap
import Types.Player
import Types.SmoothPosition
import Types.FloorTile
import Types.Items

updateInteractions :: Map -> Map
updateInteractions = updatePlayerInteractions

updatePlayerInteractions :: Map -> Map
updatePlayerInteractions
  = updatePlayerCollisionInteractions
  . updatePlayerItemInteractions

updatePlayerCollisionInteractions :: Map -> Map
updatePlayerCollisionInteractions map = map {
  mapPlayer = if shouldSwitch then newPlayer else currentPlayer
}
  where
    currentPlayer = mapPlayer map
    newPlayer = currentPlayer {
      playerPosition = swapSmoothPosition (playerPosition currentPlayer)
    }

    currentPlayerPosition = playerPosition currentPlayer

    -- TODO: this code is really full of magic numbers
    shouldSwitch = colliding && (smoothPositionTime (playerPosition currentPlayer) < 0.95)

    -- | Write first, optimize later :)
    colliding = not (any isTileSameAsPlayer $ mapFloorTiles map)
    isTileSameAsPlayer tile = floorTilePosition tile == smoothPositionCurrent currentPlayerPosition

updatePlayerItemInteractions :: Map -> Map
updatePlayerItemInteractions map = map {
  mapPlayer = newPlayer,
  mapItemTiles = newMapItemTiles
}
  where
    currentPlayer = mapPlayer map
    currentPlayerPosition = smoothPositionCurrent (playerPosition currentPlayer)
    isItemAtMyPosition (ItemTile itemPos _) = itemPos == currentPlayerPosition
    itemName (ItemTile _ name) = name

    -- adding the current items to the player
    itemHere = uncons $ filter isItemAtMyPosition (mapItemTiles map)
    itemNameHere = fmap (itemName . fst) itemHere
    newPlayer = maybe currentPlayer (playerAddItem currentPlayer) itemNameHere

    -- removing the items from this tile
    newMapItemTiles = filter (not . isItemAtMyPosition) (mapItemTiles map)
