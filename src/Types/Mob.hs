module Types.Mob where

import Types.Character
import Types.SmoothPosition

data MobDirection = Up | Down | Left | Right | None 
  deriving Eq

data Mob = Mob {
  mobCharacter :: Character,
  mobDirection :: MobDirection
}

initMob :: (Float, Float) -> Mob
initMob position = Mob {
  mobCharacter = initCharacter position,
  mobDirection = Down
}

mobCurrentPosition :: Mob -> (Float, Float)
mobCurrentPosition mob =
  currentSmoothPosition
  $ characterPosition
  $ mobCharacter mob

