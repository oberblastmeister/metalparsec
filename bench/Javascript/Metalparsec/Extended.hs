module Javascript.Metalparsec.Extended
  ( module Data.Functor,
    module Control.Monad,
    module Javascript.Metalparsec.Extended,
  )
where

import Control.Applicative (empty, liftA2, liftA3, many, some, (<**>))
import Control.Monad (MonadPlus)
import Control.Monad.Combinators (choice)
import Data.Functor (void, ($>))
import Data.List (foldl')
import Data.Text (Text)
import Text.Metalparsec hiding (chainPre, chainPost)

type Parser = Parsec Text () Text

token :: Text -> Parser ()
token = text

between o c p = o *> p <* c

-- oneOf = satisfy . inClass

-- noneOf = satisfy . notInClass

match :: (Monad m, Eq a) => [a] -> m a -> (a -> m b) -> m b -> m b
match xs p f def = p >>= (\x -> if elem x xs then f x else def)

skipSome :: Parser a -> Parser ()
skipSome p = void (some p)

-- some = many1

maybeP :: Parser a -> Parser (Maybe a)
maybeP = optional

fromMaybeP :: Monad m => m (Maybe a) -> m a -> m a
fromMaybeP mmx d = mmx >>= maybe d return

(<+>) :: Parser a -> Parser b -> Parser (Either a b)
p <+> q = Left <$> p <|> Right <$> q

(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) = liftA2 (:)

(<~>) :: Parser a -> Parser b -> Parser (a, b)
(<~>) = liftA2 (,)

pfoldl1 :: (b -> a -> b) -> b -> Parser a -> Parser b
pfoldl1 f k p = foldl' f k <$> some p

(>?>) :: MonadPlus m => m a -> (a -> Bool) -> m a
m >?> f = m >>= \x -> if f x then return x else empty

-- chainPre :: Parser (a -> a) -> Parser a -> Parser a
-- chainPre op p = flip (foldr ($)) <$> many op <*> p

-- chainPost :: Parser a -> Parser (a -> a) -> Parser a
-- chainPost p op = foldl' (flip ($)) <$> p <*> many op

-- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- chainl1 p op = chainPost p (flip <$> op <*> p)

-- chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- chainr1 p op = let go = p <**> ((flip <$> op <*> go) <|> pure id) in go

-- data Level s a
--   = InfixL [Parser (a -> a -> a)]
--   | InfixR [Parser (a -> a -> a)]
--   | Prefix [Parser (a -> a)]
--   | Postfix [Parser (a -> a)]

-- precedence :: [Level s a] -> Parser a -> Parser a
-- precedence levels atom = foldl' convert atom levels
--   where
--     convert x (InfixL ops) = chainl1 x (choice ops)
--     convert x (InfixR ops) = chainr1 x (choice ops)
--     convert x (Prefix ops) = chainPre (choice ops) x
--     convert x (Postfix ops) = chainPost x (choice ops)
