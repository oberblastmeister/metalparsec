module Util where

import Data.Text (Text)
import Test.Tasty.HUnit
import Text.Metalparsec

type Parser' a = Parsec Text a Text

type Parser = Parser' ()

text' :: Text -> Parser Text
text' t = text t $> t

parse :: Parser a -> Text -> Result Text a
parse p = evalParser p ()

-- | The parser should parse this string, consuming it entirely, and succeed
-- yielding the matching value.
shouldParseWith ::
  (Show a, Eq a, Show e) => Parsec Text () e a -> (Text, a) -> IO ()
p `shouldParseWith` (s, r) = case evalParser ((,) <$> p <*> rest) () s of
  Ok (r', "") -> r' @?= r
  Ok (_, lo) -> assertFailure $ "Unexpected leftover: " ++ show lo
  Fail -> assertFailure "Parse failed unexpectedly"
  Err e -> assertFailure $ "Parse threw unexpected error: " ++ show e
