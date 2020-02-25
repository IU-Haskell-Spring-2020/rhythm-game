module Render.Pacemaker where

import Data.Fixed
import Framework.Types

renderPacemaker :: Float -> StringPicture
renderPacemaker localDt = translated (320, 640) pacemaker
  where
    pacemaker = scaled scale $ texture "fff"

    dt = (localDt `mod'` pace) * (2 / pace)
    scale = 0.5 * max 0.8 (cos dt)
    pace = 0.25
