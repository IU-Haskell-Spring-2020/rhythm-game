module Render.Player where

import Framework.Types
import Types.Player
import Types.SmoothPosition

renderPlayer :: Player -> StringPicture
renderPlayer me
  = scaled (playerCurrentScale me * 0.8)
  $ translated (playerCurrentPosition me) pic
    where
      pic = texture $ if playerErrorTime me > 0 then "fff_red" else "fff"

playerCurrentScale :: Player -> Float
playerCurrentScale me = scale
  where
    scale = 0.4 + 0.2 * max 0.6 (sin (pi * smoothPositionTime (playerPosition me) / 4))
