module Types.Character where

import Data.Fixed
import Types.SmoothPosition


data Character = Character {
  characterPosition :: SmoothPosition,
  characterHandledThisLoop :: Bool,
  characterWaitBeats :: Int,
  characterDamageAnimationTime :: Float
}

initCharacter :: (Float, Float) -> Character
initCharacter pos = Character {
  characterPosition = initSmoothPositionAt pos,
  characterHandledThisLoop = False,
  characterWaitBeats = 1,
  characterDamageAnimationTime = 0
}

moveCharacter :: Float -> Int -> (Float, Float) -> Character -> Character
moveCharacter localDt delay towards me
  | not (characterHandledThisLoop me) && not (characterCanMove me) && dt > 0.5 =
    me {
      characterWaitBeats = max 0 (characterWaitBeats me - 1),
      characterHandledThisLoop = True
    }
  | characterCanMove me && dt < 0.5 =
    me {
      characterWaitBeats = delay,
      characterPosition = moveSmoothPosition towards (characterPosition me),
      characterHandledThisLoop = False
    }
  | dt < 0.5
    = me {
      characterHandledThisLoop = False
    }
  | otherwise = me
  where
    dt = localDt `mod'` 0.25 * 4

characterCanMove :: Character -> Bool
characterCanMove me = characterWaitBeats me == 0
