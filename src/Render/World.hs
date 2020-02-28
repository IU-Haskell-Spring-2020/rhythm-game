module Render.World where

import Framework.Types
import Types.World
import MeasureTime

import Render.Map
import Render.Pacemaker

renderWorld :: World -> StringPicture
renderWorld world
  = foldr combine Blank (mapPics ++ [worldClips, renderPacemaker localDt])
      where
        localDt = scaledMeasureLocalTime (worldPassedTime world)
        mapPics = map (translated (320 - 32, 320 - 32)) $ renderMap (worldMap world)

        -- we need to clip the world so it doesn't go out of the boundaries
        worldClip = translated (320, 32) $ texture "clip"
        worldClips = combine worldClip
          (translated (0, 640 - 64) worldClip)
