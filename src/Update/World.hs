module Update.World where

import Framework.Types
import MeasureTime
import Types.World

import Update.Map

updateWorld :: Float -> World -> (World, Action)
updateWorld dt world = (
  world {
    worldPassedTime = newPassedTime,
    worldMap = newWorldMap
  },
  action
  )
    where
      (newWorldMap, action) = updateMap dt localDt (worldMap world)
      newPassedTime = worldPassedTime world + dt
      localDt = scaledMeasureLocalTime (worldPassedTime world) timeScale
      timeScale = worldTimeScale world
