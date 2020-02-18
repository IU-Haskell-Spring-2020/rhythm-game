module RainbowFloor (
  RainbowFloor,
  RainbowFloor.init,
  draw,
  update
) where

import Framework.Types

data RainbowFloor = RainbowFloor {
  width :: Int,
  height :: Int,

  scale :: Float,
  distance :: Float,

  drawModulo :: Int
}

init :: RainbowFloor
init = RainbowFloor {
  width = 5,
  height = 5,
  scale = 0.1,
  distance = 70,
  drawModulo = 0
}

draw :: RainbowFloor -> StringPicture
draw me = foldr combine Blank pics
  where
    xRange = width me
    yRange = height me
    pics = [drawAt (x, y) me | x <- [0 .. xRange], y <- [0 .. yRange]]

drawAt :: (Int, Int) -> RainbowFloor -> StringPicture
drawAt (x, y) me
  | (x + y) `mod` 2 == modulo = Draw s (drawX, drawY) "fff"
  | otherwise                 = Blank
    where
      modulo = drawModulo me
      drawX = fromIntegral x * distance me
      drawY = fromIntegral y * distance me
      s = scale me

update :: Float -> RainbowFloor -> RainbowFloor
update mslt me
  | mslt < 0.5 = me { drawModulo = 0 }
  | otherwise  = me { drawModulo = 1 }

-- vim: set ts=2 sw=2 fdm=marker:
