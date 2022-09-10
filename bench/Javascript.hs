module Javascript where

import Control.DeepSeq (NFData)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.Text (Text)
import Data.Text.IO qualified
import Gauge
import Javascript.Attoparsec qualified as Attoparsec
import Javascript.Common
import Javascript.Metalparsec qualified as Metalparsec
import Text.Metalparsec qualified as Metalparsec

main :: Benchmark
main =
  bgroup
    "javascript"
    [ jsTest text "Atto" (attoParse Attoparsec.javascript),
      jsTest text "Metal" (metalParse Metalparsec.javascript)
    ]
  where
    jsTest :: (NFData rep, NFData res) => (FilePath -> IO rep) -> String -> (rep -> res) -> Benchmark
    jsTest = makeBenchmark ["bench/inputs/fibonacci.js", "bench/inputs/game.js", "bench/inputs/big.js"]

runMetal :: Text -> Metalparsec.Result Text JSProgram
runMetal = metalParse Metalparsec.javascript

attoParse :: Attoparsec.Parser a -> Text -> Maybe a
attoParse p = Attoparsec.maybeResult . Attoparsec.parse p

metalParse :: Metalparsec.Parsec Text () Text a -> Text -> Metalparsec.Result Text a
metalParse p = Metalparsec.evalParser p ()

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
