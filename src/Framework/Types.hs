module Framework.Types (
  -- reexports from engine
  Action(..),
  KeyPress(..),
  combine,
  translated,
  scaled,

  StringPicture(..),
  Picture(..),
  texture,
  unwrapMapPicture
) where

import Framework.Engine (Action(..), KeyPress(..), Picture(..))
import Math

-- | This is more convenient from the application code.
type StringPicture = Picture String

-- | Unwrap a picture, applying `f` only to the actual `Draw` calls.
unwrapMapPicture
  :: (   Float -> (Float, Float) -> a  -- ^ Original draw args
      -> Picture b                     -- ^ Modified draw (could be another pic)
     )                                 -- ^ Draw arg mapper
  -> Picture a                         -- ^ The picture to unwrap
  -> Picture b                         -- ^ Resulting picture
unwrapMapPicture _ Blank = Blank
unwrapMapPicture mapper (CombinedPicture p1 p2) =
  CombinedPicture (unwrapMapPicture mapper p1) (unwrapMapPicture mapper p2)
unwrapMapPicture mapper (Draw scale pos pic) = mapper scale pos pic

-- | Convenience function for `draw`ing pictures with scale=1 at the origin.
texture :: String -> StringPicture
texture = Draw 1 (0, 0)

class Combinable a where
  combine :: a -> a -> a

instance Combinable Action where
  combine a NoAction = a
  combine NoAction a = a
  combine a1 a2 = CombinedAction a1 a2

instance Combinable (Picture a) where
  combine p Blank = p
  combine Blank p = p
  combine p1 p2 = CombinedPicture p1 p2

-- | Transated picture
translated :: (Float, Float) -> Picture a -> Picture a
translated (dx, dy) = unwrapMapPicture mapper
  where
    mapper scale (x, y) = Draw scale (x + dx, y + dy)

-- | Scaled picture
scaled :: Float -> Picture a -> Picture a
scaled s = unwrapMapPicture mapper
  where
    mapper scale = Draw (scale * s)
