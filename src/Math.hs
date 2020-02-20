module Math where

type V2 = (Float, Float)

vAdd :: V2 -> V2 -> V2
vAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vSub :: V2 -> V2 -> V2
vSub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

vMul :: V2 -> Float -> V2
vMul (x, y) a = (x * a, y * a)
