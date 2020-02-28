module HandleEvent.Player where

import Framework.Types
import Math

import Types.Player
import Types.SmoothPosition

playerKeyVector :: KeyPress -> V2
playerKeyVector KeyUp = (0, -1)
playerKeyVector KeyDown = (0, 1)
playerKeyVector KeyLeft = (-1, 0)
playerKeyVector KeyRight = (1, 0)

handleEventPlayer :: KeyPress -> Player -> Player
handleEventPlayer key me = if playerCanMove me then movePlayer key me else me {
  playerErrorTime = 0.2
}

movePlayer :: KeyPress -> Player -> Player
movePlayer key me = me {
  playerPosition = playerNewPosition,
  playerCanMove = False,
  playerMoved = True
}
  where
    playerLastPosition = playerPosition me
    playerNewPosition = playerLastPosition {
      smoothPositionPrevious = smoothPositionCurrent playerLastPosition,
      smoothPositionCurrent = vAdd (smoothPositionCurrent playerLastPosition) (playerKeyVector key),
      smoothPositionTime = 1
    }
