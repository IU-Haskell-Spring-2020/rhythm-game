module ExampleActor (
  exampleActor
) where

import Data.Fixed

import Engine (KeyPress, Action(NoAction))
import Framework

data State = State Float (Float, Float) Float

exampleActor :: Actor State
exampleActor
  = Actor
      (State 1 (0, 0) 0)
      "resources/fff.jpg"
      draw
      handleEvent
      update


offsetQuarters = 2
bpm = 98 / 4
spb = 60 / bpm
localTime dt = (dt - offsetQuarters * spb) `mod'` spb
scaledLocalTime dt = localTime dt / spb

draw :: Actor State -> (Float, (Float, Float))
draw (Actor (State scale position _) _ _ _ _) = (scale, position)

handleEvent :: KeyPress -> Actor State -> Actor State
handleEvent _ = id

update :: Float -> Actor State -> (Actor State, Action)
update dt actor = (updateState (State newScale position newTime) actor, NoAction)
  where
    (State scale position time) = state actor
    newTime = time + dt
    newScale = (1 + abs (sin (2 * pi * slt))) * 0.1
    slt = scaledLocalTime newTime

-- vim: set ts=2 sw=2 fdm=marker:
