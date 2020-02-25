module Render.Map where

import Framework.Types
import Types
import Math
import Grid

import Render.FloorTile
import Render.Player

renderMap :: Map -> [StringPicture]
renderMap me = pic
    where
      cameraOffset = vNeg $ playerCurrentPosition (mapPlayer me)
      pic = map (gridPicToWorldPic . translated cameraOffset) (
        map renderFloorTile (mapFloorTiles me)
        ++ [ renderPlayer (mapPlayer me) ]
        )
