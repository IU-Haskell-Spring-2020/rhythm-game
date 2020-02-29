module Types.Player where

import Types.SmoothPosition
import Types.Items
import Math

data Player = Player {
  playerPosition :: SmoothPosition,

  playerCanMove :: Bool,
  playerMoved :: Bool,
  playerErrorTime :: Float,

  playerItems :: Items
}

initPlayer :: Player
initPlayer = Player {
  playerPosition = initSmoothPosition,

  playerCanMove = False,
  playerMoved = False,
  playerErrorTime = 0,

  playerItems = []
}

playerCurrentPosition :: Player -> (Float, Float)
playerCurrentPosition me = currentSmoothPosition (playerPosition me)

playerAddItem :: Player -> String -> Player
playerAddItem me item = me {
  playerItems = addItem item (playerItems me)
}
