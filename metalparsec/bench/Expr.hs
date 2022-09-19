module Expr where

import Control.DeepSeq (NFData)
import qualified Expr.Attoparsec as Attoparsec
import qualified Expr.Flatparse as Flatparse
import qualified Expr.Metalparsec as Metalparsec
import Gauge.Main
import Util

main :: Benchmark
main =
  bgroup
    "expr"
    [ test bytestring "Flat" (flatParseStateful Flatparse.expr),
      test text "Metal" (metalParse Metalparsec.expr),
      test bytestring "Atto" (attoParseByteString Attoparsec.expr)
    ]
  where
    test :: (NFData rep) => (FilePath -> IO rep) -> String -> (rep -> res) -> Benchmark
    test = makeBenchmark' ["bench/inputs/big-example.txt"]
