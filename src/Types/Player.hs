module Types.Player where

import Types.Character
import Types.SmoothPosition
import Types.Items
import Math

data Player = Player {
  playerCharacter :: Character,
  playerCanMove :: Bool,
  playerItems :: Items
}

initPlayer :: Player
initPlayer = Player {
  playerCharacter = initCharacter (0, 0),
  playerCanMove = False,
  playerItems = []
}

playerDisplayPosition :: Player -> (Float, Float)
playerDisplayPosition = intermediateSmoothPosition . characterPosition . playerCharacter

playerGridPosition :: Player -> (Float, Float)
playerGridPosition = smoothPositionCurrent . characterPosition . playerCharacter

playerAddItem :: Player -> String -> Player
playerAddItem me item = me {
  playerItems = addItem item (playerItems me)
}
