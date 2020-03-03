module Render.Player where

import Framework.Types
import Types.Character
import Types.Player
import Types.SmoothPosition

renderPlayer :: Player -> StringPicture
renderPlayer me
  = scaled (playerCurrentScale me * 0.8)
  $ translated (playerDisplayPosition me) pic
    where
      pic = texture $
       if characterDamageAnimationTime (playerCharacter me) > 0 
         then "fff_red"
         else "fff"

playerCurrentScale :: Player -> Float
playerCurrentScale me = scale
  where
    scale = 0.4 + 0.2 * 
      max 0.6 (sin (pi * characterDamageAnimationTime (playerCharacter me) / 4))
