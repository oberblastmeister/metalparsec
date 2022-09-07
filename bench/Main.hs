module Main where

import Gauge.Main
import Simple qualified

main :: IO ()
main =
  defaultMain
    [ Simple.main
    ]
