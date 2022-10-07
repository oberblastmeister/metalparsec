{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Simple.MetalparsecTH
  ( runSexp,
    runLongws,
    runNumcsv,
  )
where

import Data.Text (Text)
import Text.Metalparsec
import Text.Metalparsec.TH

type Parser e a = Parsec Text e () a

ws, open, close, ident, sexp, src :: Parser e ()
open = $(string' "(") >> ws
close = $(string' ")") >> ws
ws = many_ $ $(string' " ") <|> $(string' "\n")
ident = some_ (satisfyAscii isAsciiLetter) >> ws
sexp = branch open (some_ sexp >> close) ident
src = sexp >> eof

runSexp :: Text -> Result () ((), ())
runSexp = runParser src ()

longw, longws :: Parser () ()
longw = $(string' "thisisalongkeyword")
longws = some_ (longw >> ws) >> eof

runLongws :: Text -> Result () ((), ())
runLongws = runParser longws ()

numeral, comma, numcsv :: Parser () ()
numeral = some_ (satisfyAscii isAsciiDigit) >> ws
comma = $(string' ",") >> ws
numcsv = numeral >> many_ (comma >> numeral) >> eof

runNumcsv :: Text -> Result () ((), ())
runNumcsv = runParser numcsv ()
