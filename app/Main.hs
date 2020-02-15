module Main where

import Lib
import Engine

main :: IO ()
main
  = runSDL
      Nothing
      ["resources/fff.jpg"]
      "resources/scattered-and-lost.ogg"
      (const id)
      (\dt w -> (w, NoAction))
      (\w -> Draw (100, 100) 0)

-- vim: set ts=2 sw=2 fdm=marker:
