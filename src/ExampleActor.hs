{-# LANGUAGE ScopedTypeVariables #-}

module ExampleActor (
  ExampleActor,
  ExampleActor.init,
  draw,
  handleEvent,
  update,

  pos
) where

import Data.Fixed

import Framework.Types

data ExampleActor = ExampleActor {
  scale :: Float,
  pos :: (Float, Float),
  time :: Float
}

init :: ExampleActor
init = ExampleActor 0.3 (0, 0) 0

draw :: ExampleActor -> StringPicture
draw (ExampleActor scale pos _) = Draw scale pos "fff"

handleEvent :: KeyPress -> ExampleActor -> ExampleActor

handleEvent KeyUp me = me { pos = (x, y - 10) }
  where (x, y) = pos me

handleEvent KeyDown me = me { pos = (x, y + 10) }
  where (x, y) = pos me

handleEvent KeyLeft me = me { pos = (x - 10, y) }
  where (x, y) = pos me

handleEvent KeyRight me = me { pos = (x + 10, y) }
  where (x, y) = pos me


update :: Float -> ExampleActor -> ExampleActor
update dt (ExampleActor scale position time) = ExampleActor newScale position newTime
  where
    newTime = time + dt
    newScale = (1 + abs (sin (2 * pi * slt))) * 0.1
    slt = scaledLocalTime newTime

offsetQuarters = 2
bpm = 98 / 4
spb = 60 / bpm
localTime dt = (dt - offsetQuarters * spb) `mod'` spb
scaledLocalTime dt = localTime dt / spb

-- vim: set ts=2 sw=2 fdm=marker:
