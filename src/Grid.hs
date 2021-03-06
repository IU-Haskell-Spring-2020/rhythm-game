module Grid where

import Framework.Types

gridSize = 64

type GridPicture = Picture String

-- | Convert grid coords to world coords.
gridToWorld :: (Float, Float) -> (Float, Float)
gridToWorld (x, y) = (gridSize * x + gridSize / 2, gridSize * y + gridSize / 2)

-- | Convert a grid picture to a usual string picture.
gridPicToWorldPic :: GridPicture -> StringPicture
gridPicToWorldPic = unwrapMapPicture mapper
  where
    mapper scale pos = Draw scale (gridToWorld pos)
