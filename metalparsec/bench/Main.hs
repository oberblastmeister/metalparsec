module Main where

import Data.Text.IO qualified as TIO
import Gauge.Main
-- import Javascript qualified
import Simple qualified
import Util qualified

main :: IO ()
main = do
  -- text <- TIO.readFile "bench/inputs/testing.js"
  -- print text
  -- print $ Util.runMetal text
  defaultMain
    [ Simple.main
      -- Javascript.main
    ]
