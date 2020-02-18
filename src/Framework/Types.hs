module Framework.Types (
  -- reexports from engine
  Action(..),
  KeyPress(..),
  combine,

  StringPicture(..),
  Picture(..)
) where

import Engine (Action(..), KeyPress(..), Picture(..), combine)

-- | This is more convenient from the application code.
type StringPicture = Picture String
