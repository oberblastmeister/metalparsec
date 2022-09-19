module Javascript where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Text (Text)
import qualified Data.Text.IO
import Gauge.Main
-- import qualified Javascript.Attoparsec as Attoparsec
import qualified Javascript.Metalparsec as Metalparsec
import Util
import qualified Util

-- import qualified Javascript.MetalparsecTH as MetalparsecTH

main :: Benchmark
main =
  bgroup
    "javascript"
    [ --  jsTest text "Atto" (attoParse Attoparsec.javascript),
      jsTest Util.text "Metal" (metalParse Metalparsec.javascript)
      -- jsTest text "MetalTH" (metalParse MetalparsecTH.javascript)
    ]
  where
    jsTest :: (NFData rep, NFData res) => (FilePath -> IO rep) -> String -> (rep -> res) -> Benchmark
    jsTest = makeBenchmark ["bench/inputs/fibonacci.js", "bench/inputs/game.js", "bench/inputs/big.js"]

string :: FilePath -> IO String
string = readFile
