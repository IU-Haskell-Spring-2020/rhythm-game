module Render.World where

import Framework.Types
import Types.World
import Types.WorldMap
import Types.Player
import MeasureTime

import Render.Map
import Render.Pacemaker
import Render.ItemsHUD

renderWorld :: World -> StringPicture
renderWorld world
  = foldr combine Blank (mapPics ++ [worldClips, renderPacemaker localDt, itemsHUD])
      where
        localDt = scaledMeasureLocalTime (worldPassedTime world) (worldTimeScale world)
        mapPics = map (translated (320 - 32, 320 - 32)) $ renderMap (worldMap world)

        -- we need to clip the world so it doesn't go out of the boundaries
        worldClip = translated (320, 32) $ texture "clip"
        worldClips = combine worldClip
          (translated (0, 640 - 64) worldClip)

        -- rendering the HUDs
        itemsHUD = renderItemsHUD (playerItems $ mapPlayer $ worldMap world)
