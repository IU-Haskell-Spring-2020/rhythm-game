module Update.Player where

import Data.Fixed
import Types.Player
import Types.Character
import Types.SmoothPosition
import Update.SmoothPosition

updatePlayer :: Float -> Float -> Player -> Player
updatePlayer dt localDt =
  updatePlayerAnimationTime dt . updatePlayerMovementLimitations localDt

updatePlayerAnimationTime :: Float -> Player -> Player
updatePlayerAnimationTime dt me = me {
  playerCharacter = (playerCharacter me) {
    characterPosition = 
      updateSmoothPosition (characterPosition $ playerCharacter me) dt,
    characterDamageAnimationTime =
      max 0 (characterDamageAnimationTime (playerCharacter me) - dt)
  }
}

updatePlayerMovementLimitations :: Float -> Player -> Player
updatePlayerMovementLimitations localDt me = newMe
  where
    playerMoved = not $ characterCanMove $ playerCharacter me
    newMe
      | playerCanMoveAt localDt && not playerMoved
        = me {
          playerCanMove = True
        }
      | not (playerCanMoveAt localDt) && playerMoved
        = me {
          playerCharacter = (playerCharacter me) {
            characterWaitBeats = 0
          }
        }
      | otherwise = me

playerCanMoveAt :: Float -> Bool
playerCanMoveAt localDt = abs dt < leeway
    where
      fractions = 0.25
      leeway = 0.05

      dt = localDt `mod'` fractions - fractions / 2
