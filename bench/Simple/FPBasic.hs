module Simple.FPBasic
  ( runSexp,
    runLongws,
    runNumcsv,
  )
where

import FlatParse.Basic

ws, open, close, ident, sexp, src :: Parser () ()
ws = many_ $(switch [|case _ of " " -> pure (); "\n" -> pure ()|])
open = $(char '(') >> ws
close = $(char ')') >> ws
ident = some_ (satisfyASCII_ isLatinLetter) >> ws
sexp = branch open (some_ sexp >> close) ident
src = sexp >> eof

runSexp = runParser src

longw, longws :: Parser () ()
longw = $(string "thisisalongkeyword")
longws = some_ (longw >> ws) >> eof

runLongws = runParser longws

numeral, comma, numcsv :: Parser () ()
numeral = some_ (satisfyASCII_ isDigit) >> ws
comma = $(char ',') >> ws
numcsv = numeral >> many_ (comma >> numeral) >> eof

runNumcsv = runParser numcsv
