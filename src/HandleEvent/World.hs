module HandleEvent.World where

import Framework.Types
import Types

import HandleEvent.Map

handleEventWorld :: KeyPress -> World -> World
handleEventWorld event world = world {
  worldMap = handleEventMap event (worldMap world)
}
