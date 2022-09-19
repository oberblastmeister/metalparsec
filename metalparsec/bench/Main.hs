module Main where

import qualified Data.Text.IO as TIO
import qualified Expr
import qualified Expr.Metalparsec
import Gauge.Main
import qualified Javascript
import qualified Simple
import qualified Util

main :: IO ()
main = do
  text <- TIO.readFile "bench/inputs/testing"
  -- print text
  print $ Util.metalParse Expr.Metalparsec.expr text
  defaultMain
    [ Simple.main,
      Javascript.main,
      Expr.main
    ]
