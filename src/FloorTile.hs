module FloorTile (
  FloorTile,
  textureNames,
  FloorTile.init,
  draw,
  position
) where

import Framework.Types
import Grid

data FloorTile = FloorTile {
  position :: (Float, Float)
}

textureNames :: [(String, String)]
textureNames = [("floor", "resources/floor.jpg")]

init :: (Float, Float) -> FloorTile
init pos = FloorTile { position = pos }

draw :: FloorTile -> GridPicture
draw me = translated (position me) $ texture (64, 64) "floor"
