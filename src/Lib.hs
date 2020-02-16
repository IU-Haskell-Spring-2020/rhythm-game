module Lib
    ( run
    ) where

import Data.Fixed

import Framework
import ExampleActor
import ExampleActor2

run :: IO ()
run = runActors [
    makeActor (ExampleActor 1 (0, 0) 0),
    makeActor (ExampleActor2 (200, 200))
  ]
  "resources/scattered-and-lost.ogg"

-- vim: ts=2 sw=2 et fdm=syntax:
