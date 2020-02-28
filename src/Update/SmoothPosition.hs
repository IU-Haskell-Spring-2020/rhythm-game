module Update.SmoothPosition where

import Types.SmoothPosition

updateSmoothPosition
  :: SmoothPosition
  -> Float
  -> SmoothPosition
updateSmoothPosition pos dt =
  pos {
    smoothPositionTime = max 0 (smoothPositionTime pos - dt)
  }
