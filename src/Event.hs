module Event (Event(..), combine, KeyPress(..)) where

import Engine (combine, Combinable, KeyPress(..))

data Event
  = CombinedEvent Event Event
  | NoEvent
  | KeyPress KeyPress
  | TestEvent (Float, Float)

instance Combinable Event where
  combine e NoEvent = e
  combine NoEvent e = e
  combine e1 e2 = CombinedEvent e1 e2

-- vim: set ts=2 sw=2 fdm=marker:
