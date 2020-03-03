module Types.Mob where

import Types.Direction
import Types.Character
import Types.SmoothPosition

data Mob = Mob {
  mobCharacter :: Character,
  mobDirection :: Direction,
  mobShouldDie :: Bool
}

initDefaultMob :: (Float, Float) -> Mob
initDefaultMob position = Mob {
  mobCharacter = initCharacter position,
  mobDirection = Types.Direction.Left,
  mobShouldDie = False
}

mobDisplayPosition :: Mob -> (Float, Float)
mobDisplayPosition
  = intermediateSmoothPosition
  . characterPosition
  . mobCharacter

mobGridPosition :: Mob -> (Float, Float)
mobGridPosition
  = smoothPositionCurrent
  . characterPosition
  . mobCharacter

killMob :: Mob -> Mob
killMob me = me { mobShouldDie = True }
