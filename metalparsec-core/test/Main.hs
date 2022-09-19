import Data.Text (Text)
import Properties (properties)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Metalparsec
import Util

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ properties,
      testCase "alt" $ do
        parse alt "first" @?= Ok "first"
        parse alt "second" @?= Ok "second"
        parse alt "third" @?= Ok "third"
        parse alt "fourth" @?= Fail
    ]

alt :: Parser Text
alt = text' "first" <|> text' "second" <|> text' "third"
