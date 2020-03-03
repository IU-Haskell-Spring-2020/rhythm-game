module MeasureTime (scaledMeasureLocalTime) where

import Data.Fixed

offsetSeconds = 0.17
-- offsetSeconds = 0
offsetMeasures = 0
bpm = 98
secondsPerMeasure = 60 / (bpm / 4)
localTime dt = (dt - offsetMeasures * secondsPerMeasure + offsetSeconds) `mod'` secondsPerMeasure
normalizedLocalTime dt = localTime dt / secondsPerMeasure
scaledLocalTime dt scale = normalizedLocalTime dt * scale

-- | Scaled local time inside a measure (4 quarter notes).
-- | Returns floats in range [0; 1) for any float that is seconds from
-- | "beginning of time"
scaledMeasureLocalTime :: Float -> Float -> Float
scaledMeasureLocalTime = scaledLocalTime
