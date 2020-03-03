module Update.Mob where

import Data.Fixed
import Math

import Types.Character
import Types.Mob
import Types.SmoothPosition
import Types.Direction

import Update.SmoothPosition

mobDirectionVector :: Mob -> V2
mobDirectionVector me = directionToVector $ mobDirection me

updateMob :: Float -> Float -> Mob -> Maybe Mob
updateMob dt localDt me
  | localDt > 0.2 && mobShouldDie me = Nothing
  | otherwise
    = Just $ moveMob localDt $ me {
      mobCharacter = (mobCharacter me) {
        characterPosition =
          (updateSmoothPosition $ characterPosition $ mobCharacter me) dt
      }
    }
  where
    ldt = localDt `mod'` 0.25 * 4

moveMob :: Float -> Mob -> Mob
moveMob localDt me = me {
  mobCharacter = moveCharacter localDt 2 (mobDirectionVector me) (mobCharacter me)
}
