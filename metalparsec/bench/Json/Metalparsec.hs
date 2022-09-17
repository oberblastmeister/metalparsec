module Json.Metalparsec where

import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import Text.Metalparsec (Parsec)
import qualified Text.Metalparsec as P

-- scientifically :: (Scientific -> a) -> Parsec c e s a
-- scientifically h = do
--   !positive <-
--     ((== '+') <$> P.satisfyAscii (\c -> c == '-' || c == '+'))
--       <|> pure True

--   n <- decimal

--   let f fracDigits =
--         SP
--           (T.foldl' step n fracDigits)
--           (negate $ T.length fracDigits)
--       step a c = a * 10 + fromIntegral (ord c - 48)

--   SP c e <-
--     (P.satisfy (== '.') *> (f <$> P.takeWhile isDigit))
--       <|> pure (SP n 0)

--   let !signedCoeff
--         | positive = c
--         | otherwise = -c

--   ( P.satisfy (\w -> w == 'e' || w == 'E')
--       *> fmap (h . Sci.scientific signedCoeff . (e +)) (signed decimal)
--     )
--     <|> return (h $ Sci.scientific signedCoeff e)
-- {-# INLINE scientifically #-}

