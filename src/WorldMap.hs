module WorldMap (
  WorldMap,
  WorldMap.init,
  textureNames,
  draw,
  update,
  handleEvent
) where

import Framework.Types
import Grid
import MeasureTime
import Math
import qualified Box
import qualified TileMap
import qualified RainbowFloor
import qualified Player
import qualified FloorTile

data WorldMap = WorldMap {
  floorTiles :: [FloorTile.FloorTile],
  player :: Player.Player
}

textureNames :: [(String, String)]
textureNames = RainbowFloor.textureNames ++ FloorTile.textureNames

init :: WorldMap
init = WorldMap {
  floorTiles = TileMap.floorTiles,
  player = Player.init
}

draw :: WorldMap -> [StringPicture]
draw me = pic
    where
      cameraOffset = vNeg $ Box.position $ Player.currentBox (player me)
      pic = map (gridPicToWorldPic . translated cameraOffset) (
        map FloorTile.draw (floorTiles me)
        ++ [ Player.draw (WorldMap.player me) ]
        )

update :: Float -> Float -> WorldMap -> WorldMap
update dt localDt me = updateCollisions $ me {
  player = Player.update dt (player me)
}

handleEvent :: KeyPress -> WorldMap -> WorldMap
handleEvent event me = me {
  player = Player.handleEvent event (player me)
}

-- =============================================================================
-- Collision handling. {{{1

updateCollisions :: WorldMap -> WorldMap
updateCollisions me = me {
  player = updatePlayerCollisions me
}

updatePlayerCollisions :: WorldMap -> Player.Player
updatePlayerCollisions map = if shouldSwitch then newPlayer else player
  where
    player = WorldMap.player map
    newPlayer = player {
      Player.position = Player.lastPosition player,
      Player.lastPosition = Player.position player
    }
    -- TODO: this code is really full of magic numbers
    shouldSwitch = colliding && (Player.animationTime player < 0.95)
    -- | Write first, optimize later :)
    colliding = not (any isTileSameAsPlayer $ WorldMap.floorTiles map)
    isTileSameAsPlayer tile = FloorTile.position tile == Player.position player

