module World (
  World(..),
  World.init,
  textureNames,
  draw,
  handleEvent,
  update
) where

import Framework.Types
import MeasureTime (scaledMeasureLocalTime)
import qualified Pacemaker
import qualified WorldMap

data World = World {
  passedTime :: Float,
  worldMap :: WorldMap.WorldMap
}

-- =============================================================================
-- Exported functions {{{1

-- | World state at the beginning.
init :: World
init = World {
  passedTime = 0,
  worldMap = WorldMap.init
  }

-- | List of texture filenames which are going to be loaded for this world.
textureNames :: [(String, String)]
textureNames = ("clip", "resources/clip.jpg") : Pacemaker.textureNames ++ WorldMap.textureNames

-- | Draw the world from a world state.
draw :: World -> StringPicture
draw world
  = foldr combine Blank (mapPics ++ [worldClips, Pacemaker.draw localDt])
      where
        localDt = scaledMeasureLocalTime (passedTime world)
        mapPics = map (translated (320 - 32, 320 - 32)) $ WorldMap.draw (worldMap world)

        -- we need to clip the world so it doesn't go out of the boundaries
        worldClip = translated (320, 32) $ texture (640, 64) "clip"
        worldClips = combine worldClip
          (translated (0, 640 - 64) worldClip)


-- | Update the world from a dt.
update :: Float -> World -> (World, Action)
update dt world = (
  world {
    passedTime = newPassedTime,
    worldMap = WorldMap.update dt localDt (worldMap world)
  },
  NoAction
  )
    where
      newPassedTime = passedTime world + dt
      localDt = scaledMeasureLocalTime (passedTime world)

-- | Handle the incoming keypress.
handleEvent :: KeyPress -> World -> World
handleEvent event world = world {
  worldMap = WorldMap.handleEvent event (worldMap world)
}

-- vim: ts=2 sw=2 fdm=marker:
