module Simple.Megaparsec (runSexp, runLongws, runNumcsv) where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Char
import Text.Megaparsec

type Parser = Parsec () B.ByteString

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

char8 :: Char -> Parser ()
char8 c = () <$ single (fromIntegral (ord c))

satisfy8 :: (Char -> Bool) -> Parser ()
satisfy8 f = () <$ satisfy (f . chr . fromIntegral)

ws, open, close, ident, sexp :: Parser ()
ws = skipMany (satisfy8 \c -> c == ' ' || c == '\n')
open = char8 '(' >> ws
close = char8 ')' >> ws
ident = skipSome (satisfy8 isLatinLetter) <* ws
sexp = (open *> skipSome sexp <* close) <|> ident

runSexp = runParser sexp ""

longw, longws :: Parser ()
longw = do _ <- chunk "thisisalongkeyword"; pure ()
longws = skipSome (longw *> ws) <* eof

runLongws = runParser longws ""

numeral, comma, numcsv :: Parser ()
numeral = skipSome (satisfy8 \c -> '0' <= c && c <= '9') >> ws
comma = single (fromIntegral (ord ',')) >> ws
numcsv = numeral >> skipSome (comma >> numeral) >> eof

runNumcsv = runParser numcsv ""
