module Main where

import Data.Text.IO qualified as TIO
import Gauge.Main
import Javascript qualified
import Simple qualified

main :: IO ()
main = do
  text <- TIO.readFile "bench/inputs/testing.js"
  print text
  print $ Javascript.runMetal text
  defaultMain
    [ Simple.main,
      Javascript.main
    ]
