module HandleEvent.Map where

import Framework.Types
import Types

import HandleEvent.Player

handleEventMap :: KeyPress -> Map -> Map
handleEventMap event me = me {
  mapPlayer = handleEventPlayer event (mapPlayer me)
}

