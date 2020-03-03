module Interactions where

import Data.List
import Data.Maybe

import Framework.Types
import Math

import Types.Direction
import Types.Character
import Types.WorldMap
import Types.Player
import Types.SmoothPosition
import Types.FloorTile
import Types.Items
import Types.Mob

updateInteractions :: Map -> (Map, Action)
updateInteractions map0 = (map2, action)
  where
    (map1, action) = updateMobInteractions map0
    map2 = updatePlayerInteractions map1

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

updateMobInteractions :: Map -> (Map, Action)
updateMobInteractions me = (
    me {
      mapMobs = newMobs,
      mapPlayer = (mapPlayer me) {
        playerCharacter = (playerCharacter $ mapPlayer me) {
          characterDamageAnimationTime
            = if or playerCollisions
                 then 0.25
                 else characterDamageAnimationTime $ playerCharacter $ mapPlayer me
        }
      }
    },
    if or playerCollisions
       then PlayOooEffect
       else NoAction
  )
  where
    (newMobs, playerCollisions) = unzip $ map (updateMobInteraction me) (mapMobs me)

updateMobInteraction :: Map -> Mob -> (Mob, Bool)
updateMobInteraction map me = (newMob, playerCollision)
  where
    newMob = me {
      mobDirection
        = estimateDirection
            (vSub
              (playerGridPosition $ mapPlayer map)
              (mobGridPosition me)),
      mobCharacter = (mobCharacter me) {
        characterPosition
          = if playerCollision
               then swapSmoothPosition $ characterPosition $ mobCharacter me
               else characterPosition $ mobCharacter me
      }
    }

    currentPlayerPosition
      = characterPosition
      $ playerCharacter
      $ mapPlayer map

    -- TODO: this code is really full of magic numbers
    playerCollision = colliding && (smoothPositionTime currentPlayerPosition < 0.95)

    currentPlayerGridPosition = playerGridPosition $ mapPlayer map
    myGridPosition = mobGridPosition me
    colliding = currentPlayerGridPosition == myGridPosition
