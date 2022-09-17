module Main where

import qualified Data.Text.IO as TIO
import Gauge.Main
-- import qualified Javascript
import qualified Simple
import qualified Util

main :: IO ()
main = do
  -- text <- TIO.readFile "bench/inputs/testing.js"
  -- print text
  -- print $ Util.runMetal text
  defaultMain
    [ Simple.main
      -- Javascript.main
    ]
