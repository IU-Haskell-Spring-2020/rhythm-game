module Update.Map where

import Types.WorldMap

import Update.Player
import Interactions

updateMap :: Float -> Float -> Map -> Map
updateMap dt localDt me = updateInteractions $ me {
  mapPlayer = updatePlayer dt localDt (mapPlayer me)
}
