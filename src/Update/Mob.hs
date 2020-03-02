module Update.Mob where

import Math

import Types.Character
import Types.Mob
import Types.SmoothPosition
import Types.Direction

import Update.SmoothPosition

mobDirectionVector :: Mob -> V2
mobDirectionVector me = directionToVector $ mobDirection me

updateMob :: Float -> Float -> Mob -> Mob
updateMob dt localDt me = moveMob localDt $ me {
  mobCharacter = (mobCharacter me) {
    characterPosition =
      (updateSmoothPosition $ characterPosition $ mobCharacter me) dt
  }
}

moveMob :: Float -> Mob -> Mob
moveMob localDt me
  | mobMoved && dt > 0.5 =
    me { 
      mobCharacter = (mobCharacter me) { characterMoved = False }
    }
  | not mobMoved && dt < 0.5 = 
    me {
      mobCharacter = 
        (mobCharacter me) { 
          characterMoved = True,
          characterPosition = {
            -- СЧИТАЕМ ВОТ ЭТО
          }
        }
    }
  where
    dt = localDt `mod'` 0.25 * 4
    mobMoved = characterMoved $ mobCharacter me
    mobPositionPrevious