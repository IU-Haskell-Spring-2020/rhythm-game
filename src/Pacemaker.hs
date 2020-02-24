module Pacemaker (
  textureNames,
  draw,
) where

import Data.Fixed
import Framework.Types

-- | This means that it should beat every 4th note.
pace = 0.25

textureNames :: [(String, String)]
textureNames = [("fff", "resources/fff.jpg")]

draw :: Float -> StringPicture
draw localDt = translated (320, 640) pacemaker
  where
    pacemaker = scaled scale $ texture (64, 64) "fff"

    dt = (localDt `mod'` pace) * (2 / pace)
    scale = max 0.8 (cos dt)
