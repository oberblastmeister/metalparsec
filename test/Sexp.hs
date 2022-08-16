{-# LANGUAGE TemplateHaskell #-}

module Sexp where

import Data.Text (Text)
import Text.Metalparsec
import Text.Metalparsec.TH
import Text.Metalparsec.Chunk (ByteChunk)

type P e a = Parsec Text Pos () e a

open :: P e ()
open = $$(string "(") >> ws

ws :: P e ()
ws = many_ $ $$(string " ") <|> $$(string "\n")

-- close :: P e ()
-- close = $$(string ")") >> (ws)
-- identChar = satisfyAscii isLatinLetter
-- identChar = $$(string @Pos "bruh")

ident :: P e ()
ident = some_ (satisfyAscii isLatinLetter) >> ws

sexp :: P e ()
sexp = branch open (some_ sexp >> close) ident
  where
    close = $$(string ")") >> (ws)

src :: P e ()
src = sexp >> eof

runSexp :: Text -> Result () ((), ())
runSexp = runParser src ()

longw, longws :: P e ()
longw = $$(string "thisisalongkeyword")
longws = some_ (longw >> ws) >> eof

runLongws :: Text -> Result () ((), ())
runLongws = runParser longws ()

numeral :: P e ()
numeral = some_ (satisfyAscii isAsciiDigit) >> ws

comma :: P e ()
comma = $$(string ",") >> ws

numcsv :: P e ()
numcsv = numeral >> many_ (comma >> numeral) >> eof

-- thingy :: ByteChunk chunk p => Parsec chunk p u e ()
-- thingy = $(string' ",")

runNumcsv :: Text -> Result () ((), ())
runNumcsv = runParser numcsv ()
