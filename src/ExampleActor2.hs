module ExampleActor2 (
  ExampleActor2(..),
  Actor(..)
) where

import Event
import Framework

data ExampleActor2 = ExampleActor2 (Float, Float)

instance Actor ExampleActor2 where
  texturePath = const "resources/fff.jpg"
  drawFunc (ExampleActor2 pos) = (0.05, pos)
  updateFunc (ExampleActor2 pos) _ = (ExampleActor2 pos, TestEvent pos)

-- vim: set ts=2 sw=2 fdm=marker:
