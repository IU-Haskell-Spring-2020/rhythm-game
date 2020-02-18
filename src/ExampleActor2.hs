module ExampleActor2 (
  ExampleActor2,
  ExampleActor2.init,
  draw,
  handleEvent,
  update,

  pos
) where

import Framework.Types

data ExampleActor2 = ExampleActor2 {
  pos :: (Float, Float)
}

init :: ExampleActor2
init = ExampleActor2 (100, 100)

draw :: ExampleActor2 -> StringPicture
draw (ExampleActor2 pos) = Draw 0.05 pos "fff"

update :: ExampleActor2 -> ExampleActor2
update = id

handleEvent :: KeyPress -> ExampleActor2 -> ExampleActor2
handleEvent = const id

-- vim: set ts=2 sw=2 fdm=marker:
