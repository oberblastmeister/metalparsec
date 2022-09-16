import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Text.Metalparsec

type Parser' = Parsec Text Text

type Parser = Parser' ()

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ testCase "alt" $ do
        parse alt "first" @?= Ok "first"
        parse alt "second" @?= Ok "second"
        parse alt "third" @?= Ok "third"
        parse alt "fourth" @?= Fail
    ]

text' :: Text -> Parser Text
text' t = text t $> t

alt :: Parser Text
alt = text' "first" <|> text' "second" <|> text' "third"

parse :: Parser a -> Text -> Result Text a
parse p = runParser p ()
