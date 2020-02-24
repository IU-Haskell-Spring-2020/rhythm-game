-- | Box.hs
-- Bounding box type for entities in the game, along with some helper functions.

module Box (
  Box,
  position,
  dimensions,
  box,
) where

import Math

-- | Box type, basically a rectangle.
data Box = Box {
  -- | (x, y) position
  position :: V2,
  -- | (width, height) dimensions
  dimensions :: V2
}

-- | Create a box with specified dimensions, located at the origin.
box :: V2 -> Box
box dim = Box { position = (0, 0), dimensions = dim }
