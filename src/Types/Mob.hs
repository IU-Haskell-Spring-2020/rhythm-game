module Types.Mob where

import Types.Direction
import Types.Character
import Types.SmoothPosition

data Mob = Mob {
  mobCharacter :: Character,
  mobDirection :: Direction
}

initDefaultMob :: (Float, Float) -> Mob
initDefaultMob position = Mob {
  mobCharacter = initCharacter position,
  mobDirection = None
}

mobCurrentPosition :: Mob -> (Float, Float)
mobCurrentPosition mob =
  currentSmoothPosition
  $ characterPosition
  $ mobCharacter mob

