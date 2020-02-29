module Types.Direction where

import Prelude hiding (Left, Right)
import Math

data Direction = Up | Down | Left | Right | None
  deriving Eq
  
directionToVector :: Direction -> V2
directionToVector Up = (0, -1)
directionToVector Down = (0, 1)
directionToVector Left = (-1, 0)
directionToVector Right = (1, 0)
directionToVector None = (0, 0)


