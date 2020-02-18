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
import qualified ExampleActor
import qualified ExampleActor2
import qualified RainbowFloor

data World = World {
  exampleActor :: ExampleActor.ExampleActor,
  exampleActor2 :: ExampleActor2.ExampleActor2,
  floor :: RainbowFloor.RainbowFloor,

  passedTime :: Float
}

-- =============================================================================
-- Exported functions {{{1

-- | World state at the beginning.
init :: World
init = World {
  exampleActor = ExampleActor.init,
  exampleActor2 = ExampleActor2.init,
  World.floor = RainbowFloor.init,

  passedTime = 0
  }

-- | List of texture filenames which are going to be loaded for this world.
textureNames :: [(String, String)]
textureNames = [
  ("fff", "resources/fff.jpg")
  ]

-- | Draw the world from a world state.
draw :: World -> StringPicture
draw world
  = foldr combine Blank [
      ExampleActor.draw (exampleActor world),
      ExampleActor2.draw (exampleActor2 world),
      translated (200, 200) $ RainbowFloor.draw (World.floor world)
    ]

-- | Update the world from a dt.
update :: Float -> World -> (World, Action)
update dt world = (
  updateWorldCollision $ world {
    exampleActor = ExampleActor.update newPassedTime $ exampleActor world,
    exampleActor2 = ExampleActor2.update $ exampleActor2 world,
    World.floor = RainbowFloor.update smlt $ World.floor world,

    passedTime = newPassedTime
  },
  NoAction
  )
    where
      newPassedTime = passedTime world + dt
      smlt = scaledMeasureLocalTime (passedTime world)

updateWorldCollision :: World -> World
updateWorldCollision world = world { exampleActor = newExampleActor }
  where
    (x1, y1) = ExampleActor.pos $ exampleActor world
    (x2, y2) = ExampleActor2.pos $ exampleActor2 world
    newX = min x1 x2
    newY = min y1 y2
    newExampleActor = (exampleActor world) { ExampleActor.pos = (newX, newY) }

-- | Handle the incoming keypress.
handleEvent :: KeyPress -> World -> World
handleEvent event world = world {
    exampleActor = ExampleActor.handleEvent event $ exampleActor world,
    exampleActor2 = ExampleActor2.handleEvent event $ exampleActor2 world
  }

-- vim: ts=2 sw=2 fdm=marker:
