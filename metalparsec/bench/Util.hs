module Util where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.Text (Text)
import qualified Text.Metalparsec as Metalparsec

-- runMetal :: Text -> Metalparsec.Result Text JSProgram
-- runMetal = metalParse Metalparsec.javascript

-- attoParse :: Attoparsec.Parser a -> Text -> Maybe a
-- attoParse p = Attoparsec.maybeResult . Attoparsec.parse p

-- metalParse :: Metalparsec.Parsec Text () Text a -> Text -> Metalparsec.Result Text a
-- metalParse p = Metalparsec.evalParser p ()
