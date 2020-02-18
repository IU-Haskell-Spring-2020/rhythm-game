module MeasureTime (scaledMeasureLocalTime) where

import Data.Fixed

offsetMeasures = 0.5
bpm = 98
secondsPerMeasure = 60 / (bpm / 4)
localTime dt = (dt - offsetMeasures * secondsPerMeasure) `mod'` secondsPerMeasure
scaledLocalTime dt = localTime dt / secondsPerMeasure

-- | Scaled local time inside a measure (4 quarter notes).
-- | Returns floats in range [0; 1) for any float that is seconds from
-- | "beginning of time"
scaledMeasureLocalTime :: Float -> Float
scaledMeasureLocalTime dt = localTime dt / secondsPerMeasure

