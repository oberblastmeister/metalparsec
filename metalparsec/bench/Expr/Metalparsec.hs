{-# LANGUAGE Strict #-}

module Expr.Metalparsec where

import Data.Text (Text)
import Data.Void
import Expr.Common
import Text.Metalparsec

type Parser = Parsec Text () Void

expr :: Parser Expr
expr = chainl1 prod (Bin <$> op)
  where
    op = lexeme (asciiChar '+' $> Add <|> asciiChar '-' $> Sub)

prod :: Parser Expr
prod = chainl1 atom (Bin <$> op)
  where
    op = lexeme (asciiChar '*' $> Mul <|> asciiChar '/' $> Div)

atom :: Parser Expr
atom =
  (Num . fromIntegral <$> lexeme readInt)
    <|> (lexeme (asciiChar '(') *> expr <* lexeme (asciiChar ')'))

lexeme :: Parser a -> Parser a
lexeme p = p <* many_ (satisfyAscii (\c -> c == ' ' || c == '\n' || c == '\t'))
{-# INLINE lexeme #-}
