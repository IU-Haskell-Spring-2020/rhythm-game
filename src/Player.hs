module Player (
  Player,
  Player.init,
  update,
  draw,
  handleEvent
) where

import Framework.Types
import Grid
import Math

data Player = Player {
  lastPosition :: (Float, Float),
  position :: (Float, Float),
  animationTime :: Float
}

init :: Player
init = Player {
  lastPosition = (0, 0),
  position = (0, 0),
  animationTime = 0
}

draw :: Player -> GridPicture
draw me = translated pos $ scaled scale $ texture "fff"
  where
    scale = 0.4 + 0.1 * max 0.6 (sin (pi * animationTime me / 2))

    transitionTime = min 1 (sqrt ((1 - animationTime me) * 2))

    dPos = vMul (vSub (position me) (lastPosition me)) transitionTime
    pos = vAdd (lastPosition me) dPos

update :: Float -> Player -> Player
update dt me = me {
  animationTime = max 0 (animationTime me - dt)
}

keyVector :: KeyPress -> V2
keyVector KeyUp = (0, -1)
keyVector KeyDown = (0, 1)
keyVector KeyLeft = (-1, 0)
keyVector KeyRight = (1, 0)

handleEvent :: KeyPress -> Player -> Player
handleEvent key me = me {
  lastPosition = position me,
  position = vAdd (position me) (keyVector key),
  animationTime = 1
}
