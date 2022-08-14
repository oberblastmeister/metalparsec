module Text.Metalparsec.Combinators where

import Data.Bits ((.&.))
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified
import Data.Text.Internal qualified
import GHC.Base (unsafeChr)
import GHC.Exts
import GHC.Word
import Text.Metalparsec.Chunk (ByteChunk, Chunk)
import Text.Metalparsec.Chunk qualified as Chunk
import Text.Metalparsec.Parser

-- | Check that the input has at least the given number of bytes.
ensureLen :: Int -> Parser s p u e ()
ensureLen (I# len) = Parser $ \_s l p i u ->
  case len <=# l of
    1# -> OK# p i u ()
    _ -> Fail#
{-# INLINE ensureLen #-}

-- | Unsafely read a concrete byte from the input. It's not checked that the input has
unsafeScan1 :: forall chunk p u e. Chunk chunk p => Chunk.Token chunk -> Parser chunk p u e ()
unsafeScan1 t =
  Parser
    ( \s _l p i u ->
        case Chunk.unsafeIndex# (proxy# @chunk) s i of
          t' | t == t' -> OK# (Chunk.nextTokenPos t p) (i +# 1#) u ()
          _ -> Fail#
    )
{-# INLINE unsafeScan1 #-}

-- | Parse an ASCII `Char` for which a predicate holds. Assumption: the predicate must only return
--   `True` for ASCII-range characters. Otherwise this function might read a 128-255 range byte,
--   thereby breaking UTF-8 decoding.
satisfyAscii :: forall chunk p u e. ByteChunk chunk p => (Char -> Bool) -> Parser chunk p u e Char
satisfyAscii f = Parser $ \s l p i u -> case l ==# i of
  1# -> Fail#
  _ -> case Chunk.unsafeIndex# (proxy# @chunk) s i of
    b
      | (b .&. 0b10000000 == 0b00000000) && f (unsafeChr (fromIntegral b)) ->
          OK#
            ( Chunk.nextCharPos
                (proxy# @chunk)
                (Chunk.nextTokenPos b p)
            )
            (i +# 1#)
            u
            (unsafeChr (fromIntegral b))
      | otherwise -> Fail#
{-# INLINE satisfyAscii #-}

-- | Template function, creates a @Parser r e ()@ which unsafely scans a given
--   sequence of bytes.
encodeCharUtf8 :: Char -> [Word8]
encodeCharUtf8 = toList . textToByteArray . T.singleton

textToByteArray :: Text -> ByteArray
textToByteArray (Data.Text.Internal.Text (Data.Text.Array.ByteArray arr) _ _) = ByteArray arr

-- | Run a parser zero or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
many :: Parser s p u e a -> Parser s p u e [a]
many (Parser f) = Parser go
  where
    go s l p i u = case f s l p i u of
      OK# p i u a -> case go s l p i u of
        OK# p i u as -> OK# p i u (a : as)
        x -> x
      Fail# -> OK# p i u []
      Err# e -> Err# e
{-# INLINE many #-}

-- | Skip a parser zero or more times.
many_ :: Parser s p u e a -> Parser s p u e ()
many_ (Parser f) = Parser go
  where
    go s l p i u = case f s l p i u of
      OK# p i u _ -> go s l p i u
      Fail# -> OK# p i u ()
      Err# e -> Err# e
{-# INLINE many_ #-}

-- | Run a parser one or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
some :: Parser s p u e a -> Parser s p u e [a]
some p = (:) <$> p <*> many p
{-# INLINE some #-}

-- | Skip a parser one or more times.
some_ :: Parser s p u e a -> Parser s p u e ()
some_ pa = pa >> many_ pa
{-# INLINE some_ #-}

-- | Choose between two parsers. If the first parser fails, try the second one, but if the first one
--   throws an error, propagate the error.
(<|>) :: Parser s p u e a -> Parser s p u e a -> Parser s p u e a
(<|>) (Parser f) (Parser g) = Parser $ \s l p i u ->
  case f s l p i u of
    Fail# -> g s l p i u
    x -> x
{-# INLINE (<|>) #-}

infixr 6 <|>

-- | Branch on a parser: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parser s p i u a -> Parser s p i u b -> Parser s p i u b -> Parser s p i u b
branch pa pt pf = Parser $ \s l p i u -> case runParser# pa s l p i u of
  OK# p i u _ -> runParser# pt s l p i u
  Fail# -> runParser# pf s l p i u
  Err# e -> Err# e
{-# INLINE branch #-}

-- | Succeed if the input is empty.
eof :: Parser s p u e ()
eof = Parser $ \_s l p i u -> case l ==# i of
  1# -> OK# p i u ()
  _ -> Fail#
{-# INLINE eof #-}

runParser ::
  forall chunk p u e a.
  Chunk chunk p =>
  Parser chunk p u e a ->
  u ->
  chunk ->
  Result e (a, u, Chunk.ChunkSlice chunk)
runParser (Parser f) u s = case Chunk.toSlice# @chunk s of
  Chunk.Slice# (# s#, off#, len# #) ->
    case f s# len# (Chunk.defPos (proxy# @(Chunk.Token chunk))) off# u of
      Err# e -> Err e
      Fail# -> Fail
      OK# _p i _u a -> OK (a, u, Chunk.convertSlice# (Chunk.Slice# (# s#, i, len# #)))
{-# INLINEABLE runParser #-}

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# INLINE isLatinLetter #-}

isAsciiDigit :: Char -> Bool
isAsciiDigit c = '0' <= c && c <= '9'
{-# INLINE isAsciiDigit #-}
