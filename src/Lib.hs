module Lib
    ( run
    ) where

import Data.Fixed

import ExampleActor
import Framework

run :: IO ()
run = runActors [exampleActor] "resources/scattered-and-lost.ogg"

-- vim: ts=2 sw=2 et fdm=syntax:
