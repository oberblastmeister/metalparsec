{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr.Flatparse where

import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Void
import Expr.Common
import qualified FlatParse.Stateful as F

type Parser = F.Parser () Void

{-# INLINE expr #-}
expr :: Parser Expr
expr = chainl1 prod (Bin <$> op)
  where
    op = lexeme $(F.switch [|case _ of "+" -> pure Add; "-" -> pure Sub|])

{-# INLINE prod #-}
prod :: Parser Expr
prod = chainl1 atom (Bin <$> op)
  where
    op = lexeme $(F.switch [|case _ of "*" -> pure Mul; "/" -> pure Div|])

{-# INLINE atom #-}
atom :: Parser Expr
atom =
  (Num . fromIntegral <$> lexeme F.readInt)
    F.<|> (lexeme $(F.char '(') *> expr <* lexeme $(F.char ')'))

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme p = p <* F.many_ (F.satisfy_ isSpace)

{-# INLINE chainl1 #-}
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= go
  where
    go l =
      step l F.<|> pure l
    step l = do
      c <- op
      r <- p
      go (c l r)

parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
  content <- BS.readFile filepath
  pure $
    F.runParser (expr <* F.eof) () 0 content & \case
      F.OK ans _ _ -> Just ans
      _ -> Nothing
