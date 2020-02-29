module Render.Map where

import Framework.Types

import Types.WorldMap
import Types.SmoothPosition
import Types.Player

import Math
import Grid

import Render.FloorTile
import Render.ItemTile
import Render.Player

renderMap :: Map -> [StringPicture]
renderMap me = pic
    where
      cameraOffset = vNeg $ currentSmoothPosition (playerPosition (mapPlayer me))
      pic = map (gridPicToWorldPic . translated cameraOffset) (
        map renderFloorTile (mapFloorTiles me)
        ++ map renderItemTile (mapItemTiles me)
        ++ [ renderPlayer (mapPlayer me) ]
        )
