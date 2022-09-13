module Util where

import Data.Attoparsec.Text qualified as Attoparsec
import Data.Text (Text)
import Javascript.Common
import Javascript.Metalparsec qualified as Metalparsec
import Text.Metalparsec qualified as Metalparsec

runMetal :: Text -> Metalparsec.Result Text JSProgram
runMetal = metalParse Metalparsec.javascript

attoParse :: Attoparsec.Parser a -> Text -> Maybe a
attoParse p = Attoparsec.maybeResult . Attoparsec.parse p

metalParse :: Metalparsec.Parsec Text () Text a -> Text -> Metalparsec.Result Text a
metalParse p = Metalparsec.evalParser p ()
