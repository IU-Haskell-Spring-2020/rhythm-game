module HandleEvent.World where

import Framework.Types
import Types.World

import HandleEvent.Map

handleEventWorld :: KeyPress -> World -> World
handleEventWorld event world = world {
  worldMap = handleEventMap event (worldMap world)
}
