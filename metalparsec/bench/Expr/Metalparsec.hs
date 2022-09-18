{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr.Metalparsec where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Void
import Expr.Common
import Text.Metalparsec

type Parser = Parsec Text () Void

{-# INLINE expr #-}
expr :: Parser Expr
expr = chainl1 prod (Bin <$> op)
  where
    op = lexeme $ asciiChar '+' $> Add <|> asciiChar '-' $> Sub

{-# INLINE prod #-}
prod :: Parser Expr
prod = chainl1 atom (Bin <$> op)
  where
    op = lexeme $ asciiChar '*' $> Mul <|> asciiChar '/' $> Div

{-# INLINE atom #-}
atom :: Parser Expr
atom =
  (Num . fromIntegral <$> lexeme F.readInt)
    <|> (lexeme (asciiChar '(') *> expr <* lexeme (asciiChar ')'))

lexeme :: Parser a -> Parser a
lexeme p = p <* many_ (satisfyAscii isSpace)
{-# INLINE lexeme #-}
