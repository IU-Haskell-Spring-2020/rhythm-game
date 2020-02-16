{-# LANGUAGE ScopedTypeVariables #-}

module ExampleActor (
  ExampleActor(..)
) where

import Data.Fixed

import Event
import Framework

import ExampleActor2

data ExampleActor = ExampleActor Float (Float, Float) Float

instance Actor ExampleActor where
  texturePath = const "resources/fff.jpg"

  drawFunc (ExampleActor scale position _) = (scale, position)

  handleEventFunc (ExampleActor scale (x, y) time) (TestEvent (tx, ty))
    = ExampleActor scale (min x tx, min y ty) time
  handleEventFunc (ExampleActor scale (x, y) time) (KeyPress KeyRight)
    = ExampleActor scale (x + 10, y) time
  handleEventFunc (ExampleActor scale (x, y) time) (KeyPress KeyDown)
    = ExampleActor scale (x, y + 10) time
  handleEventFunc self _ = self

  updateFunc (ExampleActor scale position time) dt = (newActor, NoEvent)
    where
      newTime = time + dt
      newScale = (1 + abs (sin (2 * pi * slt))) * 0.1
      slt = scaledLocalTime newTime
      newActor = ExampleActor newScale position newTime

offsetQuarters = 2
bpm = 98 / 4
spb = 60 / bpm
localTime dt = (dt - offsetQuarters * spb) `mod'` spb
scaledLocalTime dt = localTime dt / spb

-- vim: set ts=2 sw=2 fdm=marker:
