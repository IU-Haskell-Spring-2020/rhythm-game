module Render.Map where

import Framework.Types

import Types.WorldMap
import Types.SmoothPosition
import Types.Player

import Math
import Grid

import Render.FloorTile
import Render.ItemTile
import Types.Character
import Render.Player
import Render.Projectile

renderMap :: Map -> [StringPicture]
renderMap me = pic
    where
      cameraOffset = vNeg 
        $ currentSmoothPosition 
        $ characterPosition 
        $ playerCharacter (mapPlayer me)
      pic = map (gridPicToWorldPic . translated cameraOffset) (
        map renderFloorTile (mapFloorTiles me)
        ++ map renderProjectile (mapProjectiles me)
        ++ map renderItemTile (mapItemTiles me)
        ++ [ renderPlayer (mapPlayer me) ]
        )
