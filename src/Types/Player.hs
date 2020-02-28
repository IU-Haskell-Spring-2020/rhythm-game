module Types.Player where

import Types.SmoothPosition
import Math

data Player = Player {
  playerPosition :: SmoothPosition,

  playerCanMove :: Bool,
  playerMoved :: Bool,
  playerErrorTime :: Float
}

initPlayer :: Player
initPlayer = Player {
  playerPosition = initSmoothPosition,

  playerCanMove = False,
  playerMoved = False,
  playerErrorTime = 0
}

playerCurrentPosition :: Player -> (Float, Float)
playerCurrentPosition me = currentSmoothPosition (playerPosition me)

