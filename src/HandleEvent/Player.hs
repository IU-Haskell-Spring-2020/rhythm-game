module HandleEvent.Player where

import Framework.Types
import Types
import Math

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
  playerLastPosition = playerPosition me,
  playerPosition = vAdd (playerPosition me) (playerKeyVector key),
  playerAnimationTime = 1,
  playerCanMove = False,
  playerMoved = True
}
