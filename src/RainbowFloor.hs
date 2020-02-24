module RainbowFloor (
  RainbowFloor,
  RainbowFloor.init,
  textureNames,
  draw,
  update
) where

import Framework.Types

data RainbowFloor = RainbowFloor {
  width :: Int,
  height :: Int,

  drawModulo :: Int
}

textureNames :: [(String, String)]
textureNames = [("floor", "resources/floor.jpg")]

init :: RainbowFloor
init = RainbowFloor {
  width = 6,
  height = 6,
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
  | (x + y) `mod` 2 == modulo = pic
  | otherwise                 = Blank
    where
      pic = translated position $ texture (32, 32) "floor"
      position = (fromIntegral x, fromIntegral y)
      modulo = drawModulo me

update :: Float -> RainbowFloor -> RainbowFloor
update mslt me
  | mslt < 0.5 = me { drawModulo = 0 }
  | otherwise  = me { drawModulo = 1 }

-- vim: set ts=2 sw=2 fdm=marker:
