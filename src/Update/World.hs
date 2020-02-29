module Update.World where

import Framework.Types
import MeasureTime
import Types.World

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
      localDt = scaledMeasureLocalTime (worldPassedTime world) timeScale
      timeScale = worldTimeScale world
