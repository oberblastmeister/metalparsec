import Data.Functor (($>))
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Metalparsec

type Parser = Parsec Text () Text

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ testCase "alt" $ do
        parse alt "first" @?= OK "first"
        parse alt "second" @?= OK "second"
        parse alt "third" @?= OK "third"
        parse alt "fourth" @?= Fail
    ]

text' :: Text -> Parser Text
text' t = text t $> t

alt :: Parser Text
alt = text' "first" <|> text' "second" <|> text' "third"

parse :: Parser a -> Text -> Result Text a
parse p = evalParser p ()
