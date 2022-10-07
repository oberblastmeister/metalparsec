module Simple.Metalparsec
  ( runSexp,
    runLongws,
    runNumcsv,
  )
where

import Data.Text (Text)
import Text.Metalparsec

type Parser e a = Parsec Text e () a

ws, open, close, ident, sexp, src :: Parser e ()
open = asciiChar '(' >> ws
close = asciiChar ')' >> ws
ws = many_ $ asciiChar ' ' <|> asciiChar '\n'
ident = some_ (satisfyAscii isAsciiLetter) >> ws
sexp = branch open (some_ sexp >> close) ident
src = sexp >> eof

runSexp :: Text -> Result () ()
runSexp = evalParser src ()

longw, longws :: Parser () ()
longw = text "thisisalongkeyword"
longws = some_ (longw >> ws) >> eof

runLongws :: Text -> Result () ()
runLongws = evalParser longws ()

numeral, comma, numcsv :: Parser () ()
numeral = some_ (satisfyAscii isAsciiDigit) >> ws
comma = asciiChar ',' >> ws
numcsv = numeral >> many_ (comma >> numeral) >> eof

runNumcsv :: Text -> Result () (())
runNumcsv = evalParser numcsv ()
