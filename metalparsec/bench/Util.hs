module Util where

import Control.DeepSeq (NFData)
-- import qualified Javascript.Attoparsec as Attoparsec

import qualified Data.Attoparsec.Text as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Text (Text)
import qualified Data.Text.IO
import Gauge.Main
import qualified Text.Metalparsec as Metalparsec

-- runMetal :: Text -> Metalparsec.Result Text JSProgram
-- runMetal = metalParse Metalparsec.javascript
makeBenchmark :: (NFData res, NFData rep) => [FilePath] -> (FilePath -> IO rep) -> String -> (rep -> res) -> Benchmark
makeBenchmark filenames load lib parser = env (traverse load filenames) (bgroup lib . (tasks filenames))
  where
    tasks filenames inputs = foldr (\f ts n -> bench f (nf parser (inputs !! n)) : ts (n + 1)) (const []) filenames 0

makeBenchmark' :: NFData rep => [FilePath] -> (FilePath -> IO rep) -> String -> (rep -> res) -> Benchmark
makeBenchmark' filenames load lib parser = env (traverse load filenames) (bgroup lib . (tasks filenames))
  where
    tasks filenames inputs = foldr (\f ts n -> bench f (whnf parser (inputs !! n)) : ts (n + 1)) (const []) filenames 0

text :: FilePath -> IO Text
text = Data.Text.IO.readFile

bytestring :: FilePath -> IO ByteString
bytestring = Data.ByteString.readFile

attoParse :: Attoparsec.Parser a -> Text -> Maybe a
attoParse p = Attoparsec.maybeResult . Attoparsec.parse p

metalParse :: Metalparsec.Parsec Text () e a -> Text -> Metalparsec.Result e a
metalParse p = Metalparsec.runParser p ()
