module Expr where

import Control.DeepSeq (NFData)
import qualified Expr.Attoparsec as Attoparsec
#ifdef FLATPARSE
import qualified Expr.Flatparse as Flatparse
#endif
import qualified Expr.Metalparsec as Metalparsec
import Gauge.Main
import Util
flatparseBenches :: [Benchmark]
#ifdef FLATPARSE
flatparseBenches = [
  test bytestring "Flat" (flatParseStateful Flatparse.expr)
                   ]
#else
flatparseBenches = []
#endif


main :: Benchmark
main =
  bgroup
    "expr"
    $ [ 
      test text "Metal" (metalParse Metalparsec.expr),
      test bytestring "Atto" (attoParseByteString Attoparsec.expr)
    ] ++ flatparseBenches
  where
    test :: (NFData rep) => (FilePath -> IO rep) -> String -> (rep -> res) -> Benchmark
    test = makeBenchmark' ["bench/inputs/big-example.txt"]
