module Render.FloorTile where

import Framework.Types
import Types.FloorTile

renderFloorTile :: FloorTile -> StringPicture
renderFloorTile me = scaled 0.5 $ translated (floorTilePosition me) $ texture "floor"
