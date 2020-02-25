module Update.World where

import Framework.Types
import MeasureTime
import Types

import Update.Map

updateWorld :: Float -> World -> (World, Action)
updateWorld dt world = (
  world {
    worldPassedTime = newPassedTime,
    worldMap = updateMap dt localDt (worldMap world)
  },
  NoAction
  )
    where
      newPassedTime = worldPassedTime world + dt
      localDt = scaledMeasureLocalTime (worldPassedTime world)
