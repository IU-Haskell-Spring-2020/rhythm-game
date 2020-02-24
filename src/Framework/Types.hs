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
import Box
import Math

-- | This is more convenient from the application code.
type StringPicture = Picture String

-- | Unwrap a picture, applying `f` only to the actual `Draw` calls.
unwrapMapPicture
  :: (   Box -> a               -- ^ Original draw args
      -> Picture b              -- ^ Modified draw (could be another pic)
     )                          -- ^ Draw arg mapper
  -> Picture a                  -- ^ The picture to unwrap
  -> Picture b                  -- ^ Resulting picture
unwrapMapPicture _ Blank = Blank
unwrapMapPicture mapper (CombinedPicture p1 p2) =
  CombinedPicture (unwrapMapPicture mapper p1) (unwrapMapPicture mapper p2)
unwrapMapPicture mapper (Draw box pic) = mapper box pic

-- | Convenience function for `draw`ing pictures with specified size at the
-- origin.
texture :: V2 -> String -> StringPicture
texture dimensions = Draw (Box.box dimensions)

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

class Translatable a where
  translated :: V2 -> a -> a

instance Translatable Box where
  translated vec box = box { Box.position = vAdd (Box.position box) vec }

instance Translatable (Picture a) where
  translated vec = unwrapMapPicture mapper
    where
      mapper box = Draw (translated vec box)

class Scalable a where
  scaled :: Float -> a -> a

instance Scalable Box where
  scaled factor box = box { Box.dimensions = vMul (Box.dimensions box) factor }

instance Scalable (Picture a) where
  scaled factor = unwrapMapPicture mapper
    where
      mapper box = Draw (scaled factor box)
