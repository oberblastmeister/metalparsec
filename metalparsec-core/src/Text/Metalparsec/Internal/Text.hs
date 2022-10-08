module Text.Metalparsec.Internal.Text where

import Control.Monad (void)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Exts
import qualified GHC.Exts as Exts
import GHC.Stack (HasCallStack)
import Text.Metalparsec.Internal
import Text.Metalparsec.Internal.Chunk (ByteChunk, Chunk)
import qualified Text.Metalparsec.Internal.SizedCompat as S
import qualified Text.Metalparsec.Internal.Utf8 as Utf8
import Text.Metalparsec.Internal.Util

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyAscii :: ByteChunk c => (Char -> Bool) -> Parsec c u e Char
satisfyAscii f = Parsec $ \(Env# c l) (Ix# o i) s ->
  STR# s $# case l ==# i of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c -> case c `leChar#` '\x7f'# of
        1# | f (C# c) -> Ok# (Ix# (o +# 1#) (i +# 1#)) (C# c)
        _ -> Fail#
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

asciiChar :: (HasCallStack, ByteChunk c) => Char -> Parsec c s e ()
asciiChar c =
  if c < '\x7f'
    then Exts.inline unsafeAsciiChar c
    else error "Text.Metalparsec.Internal.Text: not ascii char"

unsafeAsciiChar :: ByteChunk c => Char -> Parsec c s e ()
unsafeAsciiChar (C# ch) =
  Parsec $ \(Env# c l) (Ix# o i) s ->
    STR# s $# case l ==# i of
      1# -> Fail#
      _ -> case indexCharArray# c i of
        ch' -> case ch `eqChar#` ch' of
          1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) ()
          _ -> Fail#

text :: ByteChunk c => Text -> Parsec c u e ()
text (UnsafeText# bs# off# len#) = Parsec $ \(Env# c l) (Ix# o i) s ->
  STR# s $# case i +# len# <=# l of
    1# ->
      case compareByteArrays# bs# off# c i len# of
        0# -> Ok# (Ix# (o +# len#) (i +# len#)) ()
        _ -> Fail#
    _ -> Fail#

-- | Parse any UTF-8-encoded `Char`.
anyChar :: ByteChunk c => Parsec c s e Char
anyChar = Parsec $ \(Env# c l) (Ix# o i) s ->
  STR# s $# case i ==# l of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c1 -> case c1 `leChar#` '\x7F'# of
        1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) (C# c1)
        _ ->
          case (i +# 1#) ==# l of
            1# -> Fail#
            _ -> case indexCharArray# c (i +# 1#) of
              c2 -> case c1 `leChar#` '\xDF'# of
                1# -> Ok# (Ix# (o +# 2#) (i +# 2#)) (C# (Utf8.char2# c1 c2))
                _ -> case (i +# 2#) ==# l of
                  1# -> Fail#
                  _ -> case indexCharArray# c (i +# 2#) of
                    c3 -> case c1 `leChar#` '\xEF'# of
                      1# -> Ok# (Ix# (o +# 3#) (i +# 3#)) (C# (Utf8.char3# c1 c2 c3))
                      _ -> case (l +# 3#) ==# l of
                        1# -> Fail#
                        _ -> case indexCharArray# c 3# of
                          c4 -> Ok# (Ix# (o +# 4#) (i +# 4#)) (C# (Utf8.char4# c1 c2 c3 c4))

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: ByteChunk c => Parsec c s e ()
anyChar_ = Parsec $ \(Env# c l) (Ix# o i) s ->
  STR# s $# case i ==# l of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c1 ->
        case c1 `leChar#` '\x7F'# of
          1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) ()
          _ ->
            case Utf8.lengthByLeader (charToWord8 (C# c1)) of
              I# len# -> case i +# len# <# l of
                1# -> Ok# (Ix# (o +# len#) (i +# len#)) ()
                _ -> Fail#

-- | Parse any `Char` in the ASCII range, fail if the next input character is not in the range.
-- This is more efficient than `anyChar` if we are only working with ASCII.
anyCharAscii :: ByteChunk s => Parsec s u e Char
anyCharAscii = Parsec $ \(Env# c l) (Ix# o i) s ->
  STR# s $# case i ==# l of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c -> case c `leChar#` '\x7F'# of
        1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) (C# c)
        _ -> Fail#

-- | Skip any `Char` in the ASCII range. More efficient than `anyChar_` if we're working only with
-- ASCII.
anyCharAscii_ :: ByteChunk c => Parsec c u e ()
anyCharAscii_ = void anyCharAscii

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfy :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e Char
satisfy f = Parsec $ \e p s -> case runParsec# (anyChar @chunk) e p s of
  STR# s r ->
    STR# s $# case r of
      Ok# p c | f c -> Ok# p c
      _ -> Fail#
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
fusedSatisfy f1 f2 f3 f4 = Parsec $ \(Env# c l) p@(Ix# _ i) s ->
  STR# s $# case i ==# l of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c1 -> case c1 `leChar#` '\x7f'# of
        1#
          | f1 (C# c1) -> Ok# (p `plusIx#` 1#) (C# c1)
          | otherwise -> Fail#
        _ -> case i +# 1# ==# l of
          1# -> Fail#
          _ -> case indexCharArray# c (i +# 1#) of
            c2 -> case c1 `leChar#` '\xdf'# of
              1#
                | let ch = C# (Utf8.char2# c1 c2), f2 ch -> Ok# (p `plusIx#` 2#) ch
                | otherwise -> Fail#
              _ -> case i +# 2# ==# l of
                1# -> Fail#
                _ -> case indexCharArray# c (i +# 2#) of
                  c3 -> case c1 `leChar#` '\xef'# of
                    1#
                      | let ch = C# (Utf8.char3# c1 c2 c3), f3 ch -> Ok# (p `plusIx#` 3#) ch
                      | otherwise -> Fail#
                    _ -> case indexCharArray# c (i +# 3#) of
                      c4
                        | let ch = C# (Utf8.char4# c1 c2 c3 c4), f4 ch -> Ok# (p `plusIx#` 4#) ch
                        | otherwise -> Fail#
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
unsafeByte (S.W8# ch) = Parsec $ \(Env# c _) (Ix# o i) s ->
  STR#
    s
    ( case S.indexWord8Array# c i of
        ch' -> case ch `S.eqWord8#` ch' of
          1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) ()
          _ -> Fail#
    )

mul10 :: Int# -> Int#
mul10 n = uncheckedIShiftL# n 3# +# uncheckedIShiftL# n 1#
{-# INLINE mul10 #-}

readInt## :: Int# -> (# ByteArray#, Int# #) -> Int# -> (# Int#, Int# #)
readInt## acc e@(# s, l #) i = case i ==# l of
  1# -> (# acc, i #)
  _ -> case S.indexWord8Array# s i of
    w
      | 1# <- S.leWord8# (S.wordToWord8# 0x30##) w,
        1# <- S.leWord8# w (S.wordToWord8# 0x39##) ->
          readInt## (mul10 acc +# (word2Int# (S.word8ToWord# w) -# 0x30#)) e (i +# 1#)
    _ -> (# acc, i #)
{-# INLINE readInt## #-}

readInt# :: (# ByteArray#, Int# #) -> Int# -> (# (# #) | (# Int#, Int# #) #)
readInt# e i = case readInt## 0# e i of
  (# n, i' #)
    | 1# <- i ==# i' -> (# (# #) | #)
    | otherwise -> (# | (# n, i' #) #)
{-# INLINE readInt# #-}

readInt :: ByteChunk c => Parsec c s e Int
readInt = Parsec $ \(Env# c l) (Ix# o i) s ->
  STR# s $# case readInt# (# c, l #) i of
    (# (# #) | #) -> Fail#
    (# | (# n, i' #) #) -> Ok# (Ix# (o +# (i' -# i)) i') (I# n)

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
