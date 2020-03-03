module Interactions where

import Data.List
import Data.Maybe

import Math

import Types.Direction
import Types.Character
import Types.WorldMap
import Types.Player
import Types.SmoothPosition
import Types.FloorTile
import Types.Items
import Types.Mob

updateInteractions :: Map -> Map
updateInteractions = updateMobInteractions . updatePlayerInteractions

updatePlayerInteractions :: Map -> Map
updatePlayerInteractions
  = updatePlayerCollisionInteractions
  . updatePlayerItemInteractions

updatePlayerCollisionInteractions :: Map -> Map
updatePlayerCollisionInteractions map = map {
  mapPlayer = if shouldSwitch then newPlayer else currentPlayer,
  mapMobs = Prelude.map updateMob $ mapMobs map
}
  where
    currentPlayer = mapPlayer map
    newPlayer = currentPlayer {
      playerCharacter = (playerCharacter currentPlayer) {
        characterPosition = (swapSmoothPosition . characterPosition . playerCharacter) currentPlayer
      }
    }

    currentPlayerPosition = characterPosition . playerCharacter $ currentPlayer

    -- TODO: this code is really full of magic numbers
    shouldSwitch = colliding && (smoothPositionTime ((characterPosition . playerCharacter) currentPlayer) < 0.95)

    -- | Write first, optimize later :)
    colliding
      =  not (any positionSameAsPlayer $ Prelude.map floorTilePosition $ mapFloorTiles map)
      || not (null collidingMobs)
    positionSameAsPlayer = (== smoothPositionCurrent currentPlayerPosition)

    collidingMobs
      = filter positionSameAsPlayer
      $ Prelude.map mobGridPosition
      $ mapMobs map
    updateMob mob
      | positionSameAsPlayer $ mobGridPosition mob = killMob mob
      | otherwise                                    = mob

updatePlayerItemInteractions :: Map -> Map
updatePlayerItemInteractions map = map {
  mapPlayer = newPlayer,
  mapItemTiles = newMapItemTiles
}
  where
    currentPlayer = mapPlayer map
    currentPlayerPosition = smoothPositionCurrent (characterPosition $ playerCharacter currentPlayer)
    isItemAtMyPosition (ItemTile itemPos _) = itemPos == currentPlayerPosition
    itemName (ItemTile _ name) = name

    -- adding the current items to the player
    itemHere = uncons $ filter isItemAtMyPosition (mapItemTiles map)
    itemNameHere = fmap (itemName . fst) itemHere
    newPlayer = maybe currentPlayer (playerAddItem currentPlayer) itemNameHere

    -- removing the items from this tile
    newMapItemTiles = filter (not . isItemAtMyPosition) (mapItemTiles map)

updateMobInteractions :: Map -> Map
updateMobInteractions me = me {
  mapMobs = map (updateMobInteraction me) (mapMobs me)
}

updateMobInteraction :: Map -> Mob -> Mob
updateMobInteraction map me = me {
  mobDirection
    = estimateDirection
        (vSub
          (playerGridPosition $ mapPlayer map)
          (mobGridPosition me))
}
