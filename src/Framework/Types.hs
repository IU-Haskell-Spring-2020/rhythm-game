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

import Engine (Action(..), KeyPress(..), Picture(..))

-- | This is more convenient from the application code.
type StringPicture = Picture (Float, Float) String

-- | A convenience wrapper around the StringPicture.
texture :: Num a => String -> Picture (a, a) String
texture = Draw 1 (0, 0)

-- | Unwrap a picture, applying `f` only to the actual `Draw` calls.
unwrapMapPicture
  :: (   Float -> a -> b        -- ^ Original draw args
      -> Picture c d            -- ^ Modified draw (could be another pic)
     )                          -- ^ Draw arg mapper
  -> Picture a b                -- ^ The picture to unwrap
  -> Picture c d                -- ^ Resulting picture
unwrapMapPicture _ Blank = Blank
unwrapMapPicture mapper (CombinedPicture p1 p2) =
  CombinedPicture (unwrapMapPicture mapper p1) (unwrapMapPicture mapper p2)
unwrapMapPicture mapper (Draw scale pos pic) = mapper scale pos pic

class Combinable a where
  combine :: a -> a -> a

instance Combinable Action where
  combine a NoAction = a
  combine NoAction a = a
  combine a1 a2 = CombinedAction a1 a2

instance Combinable (Picture a b) where
  combine p Blank = p
  combine Blank p = p
  combine p1 p2 = CombinedPicture p1 p2

-- | Transated picture
translated :: Num a => (a, a) -> Picture (a, a) b -> Picture (a, a) b
translated (dx, dy) = unwrapMapPicture mapper
  where
    mapper scale (x, y) = Draw scale (x + dx, y + dy)

-- | Scaled picture
scaled :: Float -> Picture a b -> Picture a b
scaled s = unwrapMapPicture mapper
  where
    mapper scale = Draw (scale * s)
