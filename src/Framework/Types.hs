module Framework.Types (
  -- reexports from engine
  Action(..),
  KeyPress(..),
  combine,
  translated,

  StringPicture(..),
  Picture(..)
) where

import Engine (Action(..), KeyPress(..), Picture(..), combine, translated)

-- | This is more convenient from the application code.
type StringPicture = Picture String
