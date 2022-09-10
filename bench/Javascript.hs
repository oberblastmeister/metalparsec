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

main :: Benchmark
main =
  bgroup
    "javascript"
    [jsTest text "Atto" (attoParse Attoparsec.javascript)]
  where
    jsTest :: NFData rep => (FilePath -> IO rep) -> String -> (rep -> Maybe JSProgram) -> Benchmark
    jsTest = makeBenchmark ["bench/inputs/game.js"]

attoParse :: Attoparsec.Parser a -> Text -> Maybe a
attoParse p = Attoparsec.maybeResult . Attoparsec.parse p

-- metalParse :: Metalparsec.Parser Text u e a -> Text -> Result u e a
-- metalParse p = Metalparsec.runParser p

string :: FilePath -> IO String
string = readFile

text :: FilePath -> IO Text
text = Data.Text.IO.readFile

bytestring :: FilePath -> IO ByteString
bytestring = Data.ByteString.readFile

makeBenchmark :: (NFData a, NFData rep) => [FilePath] -> (FilePath -> IO rep) -> String -> (rep -> Maybe a) -> Benchmark
makeBenchmark filenames load lib parser = env (traverse load filenames) (bgroup lib . (tasks filenames))
  where
    tasks filenames inputs = foldr (\f ts n -> bench f (nf parser (inputs !! n)) : ts (n + 1)) (const []) filenames 0
