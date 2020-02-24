module Player (
  Player,
  Player.init,
  update,
  draw,
  handleEvent,
  currentBox,

  animationTime,
  lastPosition,
  position
) where

import Framework.Types
import Grid
import Math
import qualified Box

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
draw me = Draw (currentBox me) "fff"

currentBox :: Player -> Box.Box
currentBox me = myBox
  where
    myBox = scaled 0.8 $ scaled scale $ translated pos $ Box.box (128, 128)

    transitionTime = min 1 (sqrt ((1 - animationTime me) * 5))
    dPos = vMul (vSub (position me) (lastPosition me)) transitionTime
    pos = vAdd (lastPosition me) dPos
    scale = 0.4 + 0.2 * max 0.6 (sin (pi * animationTime me / 4))


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
