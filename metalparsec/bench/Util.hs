module Util where

import Control.DeepSeq (NFData)
-- import qualified Javascript.Attoparsec as Attoparsec

import qualified Data.Attoparsec.ByteString as Attoparsec.ByteString
import qualified Data.Attoparsec.Text as Attoparsec.Text
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Text (Text)
import qualified Data.Text.IO
import qualified FlatParse.Basic as Flatparse
import qualified FlatParse.Stateful as Flatparse.Stateful
import Gauge.Main
import qualified Text.Metalparsec as Metalparsec

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

attoParseText :: Attoparsec.Text.Parser a -> Text -> Maybe a
attoParseText p = Attoparsec.Text.maybeResult . Attoparsec.Text.parse p

attoParseByteString :: Attoparsec.ByteString.Parser a -> ByteString -> Maybe a
attoParseByteString p = Attoparsec.ByteString.maybeResult . Attoparsec.ByteString.parse p

metalParse :: Metalparsec.Parsec Text () e a -> Text -> Metalparsec.Result e a
metalParse p = Metalparsec.evalParser p ()

flatParseBasic :: Flatparse.Parser e a -> ByteString -> Flatparse.Result e a
flatParseBasic = Flatparse.runParser

flatParseStateful :: Flatparse.Stateful.Parser () e a -> ByteString -> Flatparse.Stateful.Result e a
flatParseStateful p = Flatparse.Stateful.runParser p () 0
