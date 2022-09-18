module Expr where

import qualified Expr.Metalparsec as Metalparsec
import Gauge.Main
import Util

main :: Benchmark
main =
  bgroup
    "expr"
    [ test text "Metal" (metalParse Metalparsec.expr)
    ]
  where
    test = makeBenchmark' ["bench/inputs/big-example.txt"]
