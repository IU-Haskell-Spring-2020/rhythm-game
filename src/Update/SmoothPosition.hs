module Update.SmoothPosition where

import Types.SmoothPosition

updateSmoothPosition 
  :: SmoothPosition
  -> Float
  -> SmoothPosition
updateSmoothPosition currentPosition newTime
  = currentPosition {
    smoothPositionTime = newTime
  } 
