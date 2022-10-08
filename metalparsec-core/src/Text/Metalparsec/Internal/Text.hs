module Text.Metalparsec.Internal.Text where

import Control.Monad (void)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Exts
import Text.Metalparsec.Internal
import Text.Metalparsec.Internal.Chunk (ByteChunk, Chunk)
import qualified Text.Metalparsec.Prim as Prim

lift :: Prim.Parsec c e a -> Parsec c s e a
lift (Prim.Parsec f) = Parsec $ \e (Ix# o i) s ->
  STR# s $# case f e i of
    Prim.Ok# i' x -> Ok# (Ix# (o +# (i' -# i)) i') x
    Prim.Err# e -> Err# e
    Prim.Fail# -> Fail#
{-# INLINE lift #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyAscii :: ByteChunk c => (Char -> Bool) -> Parsec c u e Char
satisfyAscii f = lift $ Prim.satisfyAscii f
{-# INLINE satisfyAscii #-}

-- | The predicate must not return true for chars that are not ascii.
unsafeSatisfyAscii :: ByteChunk c => (Char -> Bool) -> Parsec c u e Char
unsafeSatisfyAscii f = Parsec $ \(Env# c l) (Ix# o i) s ->
  STR#
    s
    ( case l ==# i of
        1# -> Fail#
        _ -> case indexCharArray# c i of
          c | f (C# c) -> Ok# (Ix# (o +# 1#) (i +# 1#)) (C# c)
          _ -> Fail#
    )
{-# INLINE unsafeSatisfyAscii #-}

char :: ByteChunk c => Char -> Parsec c s e ()
char = text . T.singleton

asciiChar :: ByteChunk c => Char -> Parsec c s e ()
asciiChar c =
  if c < '\x7f'
    then unsafeAsciiChar c
    else error "Text.Metalparsec.Internal.Text: not ascii char"

unsafeAsciiChar :: ByteChunk c => Char -> Parsec c s e ()
unsafeAsciiChar = lift . Prim.unsafeAsciiChar

text :: ByteChunk c => Text -> Parsec c u e ()
text = lift . Prim.text

-- | Parse any UTF-8-encoded `Char`.
anyChar :: ByteChunk c => Parsec c s e Char
anyChar = lift Prim.anyChar

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: ByteChunk c => Parsec c s e ()
anyChar_ = lift Prim.anyChar_

-- | Parse any `Char` in the ASCII range, fail if the next input character is not in the range.
-- This is more efficient than `anyChar` if we are only working with ASCII.
anyCharAscii :: ByteChunk s => Parsec s u e Char
anyCharAscii = lift Prim.anyCharAscii

-- | Skip any `Char` in the ASCII range. More efficient than `anyChar_` if we're working only with
-- ASCII.
anyCharAscii_ :: ByteChunk c => Parsec c u e ()
anyCharAscii_ = void anyCharAscii

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfy :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e Char
satisfy f = lift $ Prim.satisfy f
{-# INLINE satisfy #-}

-- | This is a variant of `satisfy` which allows more optimization. We can pick four testing
-- functions for the four cases for the possible number of bytes in the UTF-8 character. So in
-- @fusedSatisfy f1 f2 f3 f4@, if we read a one-byte character, the result is scrutinized with
-- @f1@, for two-bytes, with @f2@, and so on. This can result in dramatic lexing speedups.
--
-- For example, if we want to accept any letter, the naive solution would be to use
-- `Data.Char.isLetter`, but this accesses a large lookup table of Unicode character classes. We
-- can do better with @fusedSatisfy isLatinLetter isLetter isLetter isLetter@, since here the
-- `isLatinLetter` is inlined into the UTF-8 decoding, and it probably handles a great majority of
-- all cases without accessing the character table.
fusedSatisfy ::
  ByteChunk c =>
  (Char -> Bool) ->
  (Char -> Bool) ->
  (Char -> Bool) ->
  (Char -> Bool) ->
  Parsec c u e Char
fusedSatisfy f1 f2 f3 f4 = lift $ Prim.fusedSatisfy f1 f2 f3 f4
{-# INLINE fusedSatisfy #-}

isAsciiLetter :: Char -> Bool
isAsciiLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# INLINE isAsciiLetter #-}

isAsciiDigit :: Char -> Bool
isAsciiDigit c = '0' <= c && c <= '9'
{-# INLINE isAsciiDigit #-}

-- | Does not check if eof has been hit
-- This can also result in invalid utf8.
unsafeByte :: ByteChunk c => Word8 -> Parsec c s e ()
unsafeByte = lift . Prim.unsafeByte

readInt :: ByteChunk c => Parsec c s e Int
readInt = lift Prim.readInt

data LineCol = LineCol
  { lcLine :: !Int,
    lcCol :: !Int
  }

-- | Compute corresponding line and column numbers for each `Pos` in a list. Throw an error
-- on invalid positions. Note: computing lines and columns may traverse the `Chunk`,
-- but it traverses it only once regardless of the length of the position list.
zipLineCols :: forall c a. (Chunk c, ByteChunk c) => c -> [(a, Pos)] -> [(a, Pos, LineCol)]
zipLineCols str poss =
  case evalParser @c (go 0 0 sorted) () str of
    Ok res -> snd <$> List.sortOn fst res
    _ -> error "invalid position"
  where
    go :: Int -> Int -> [(Int, (a, Pos))] -> SimpleParsec c [(Int, (a, Pos, LineCol))]
    go !_line !_col [] = pure []
    go line col poss@((i, (x, pos)) : poss') = do
      p <- getPos
      if pos == p
        then ((i, (x, pos, LineCol line col)) :) <$> go line col poss'
        else do
          c <- anyChar
          if '\n' == c
            then go (line + 1) 0 poss
            else go line (col + 1) poss

    sorted :: [(Int, (a, Pos))]
    sorted = List.sortOn (snd . snd) (zip [0 ..] poss)
