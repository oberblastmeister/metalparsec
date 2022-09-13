module Javascript where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.Text (Text)
import Data.Text.IO qualified
import Gauge.Main
-- import Javascript.Attoparsec qualified as Attoparsec
import Javascript.Metalparsec qualified as Metalparsec
import Util

-- import Javascript.MetalparsecTH qualified as MetalparsecTH

main :: Benchmark
main =
  bgroup
    "javascript"
    [ --  jsTest text "Atto" (attoParse Attoparsec.javascript),
      jsTest text "Metal" (metalParse Metalparsec.javascript)
      -- jsTest text "MetalTH" (metalParse MetalparsecTH.javascript)
    ]
  where
    jsTest :: (NFData rep, NFData res) => (FilePath -> IO rep) -> String -> (rep -> res) -> Benchmark
    jsTest = makeBenchmark ["bench/inputs/fibonacci.js", "bench/inputs/game.js", "bench/inputs/big.js"]

string :: FilePath -> IO String
string = readFile

text :: FilePath -> IO Text
text = Data.Text.IO.readFile

bytestring :: FilePath -> IO ByteString
bytestring = Data.ByteString.readFile

makeBenchmark :: (NFData res, NFData rep) => [FilePath] -> (FilePath -> IO rep) -> String -> (rep -> res) -> Benchmark
makeBenchmark filenames load lib parser = env (traverse load filenames) (bgroup lib . (tasks filenames))
  where
    tasks filenames inputs = foldr (\f ts n -> bench f (nf parser (inputs !! n)) : ts (n + 1)) (const []) filenames 0
