module Types.SmoothPosition where

import Math

data SmoothPosition = SmoothPosition {
  smoothPositionPrevious :: (Float, Float),
  smoothPositionCurrent :: (Float, Float),
  smoothPositionTime :: Float
}

initSmoothPosition :: SmoothPosition
initSmoothPosition = SmoothPosition {
  smoothPositionPrevious = (0, 0),
  smoothPositionCurrent = (0, 0),
  smoothPositionTime = 0
}

initSmoothPositionAt :: (Float, Float) -> SmoothPosition
initSmoothPositionAt coords = SmoothPosition {
  smoothPositionPrevious = coords,
  smoothPositionCurrent = coords,
  smoothPositionTime = 0
}

intermediateSmoothPosition :: SmoothPosition -> (Float, Float)
intermediateSmoothPosition pos = currentPosition
  where
    transitionTime = min 1 (sqrt ((1 - smoothPositionTime pos) * 5))
    dPos = vMul (vSub (smoothPositionCurrent pos) (smoothPositionPrevious pos)) transitionTime
    currentPosition = vAdd (smoothPositionPrevious pos) dPos

swapSmoothPosition :: SmoothPosition -> SmoothPosition
swapSmoothPosition old =
  old {
    smoothPositionPrevious = smoothPositionCurrent old,
    smoothPositionCurrent = smoothPositionPrevious old
  }

moveSmoothPosition :: (Float, Float) -> SmoothPosition -> SmoothPosition
moveSmoothPosition towards me = me {
  smoothPositionPrevious = smoothPositionCurrent me,
  smoothPositionCurrent = vAdd (smoothPositionCurrent me) towards,
  smoothPositionTime = 1
}

