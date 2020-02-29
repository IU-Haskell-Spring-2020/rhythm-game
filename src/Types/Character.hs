module Types.Character where

import Types.SmoothPosition


data Character = Character {
  characterPosition :: SmoothPosition,
  characterMoved :: Bool,
  characterDamageAnimationTime :: Float
}

initCharacter :: (Float, Float) -> Character
initCharacter pos = Character {
  characterPosition = initSmoothPositionAt pos,
  characterMoved = False,
  characterDamageAnimationTime = 0
}
