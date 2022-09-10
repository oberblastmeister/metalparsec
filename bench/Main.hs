module Main where

import Gauge.Main
import Javascript qualified
import Simple qualified

main :: IO ()
main =
  defaultMain
    [ Simple.main,
      Javascript.main
    ]
