{-# LANGUAGE BlockArguments #-}

module Text.Metalparsec.Internal.Text where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Exts
import GHC.Stack (HasCallStack)
import Text.Metalparsec.Internal
import Text.Metalparsec.Internal.Chunk (ByteChunk)
import qualified Text.Metalparsec.Internal.Chunk as Chunk
import Text.Metalparsec.Internal.Combinators
import qualified Text.Metalparsec.Internal.SizedCompat as S
import qualified Text.Metalparsec.Internal.Utf8 as Utf8
import Text.Metalparsec.Internal.Util

takeWhileChar :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
takeWhileChar = manySlice . satisfyChar
{-# INLINE takeWhileChar #-}

-- -- -- this is wrong
-- -- takeWhileChar1 :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
-- -- takeWhileChar1 f = satisfyChar f *> takeWhileChar f
-- -- {-# INLINE takeWhileChar1 #-}

takeWhileAscii :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
takeWhileAscii = manySlice . satisfyAscii
{-# INLINE takeWhileAscii #-}

-- -- takeWhileAscii1 :: forall chunk u e. ByteChunk chunk => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
-- -- takeWhileAscii1 f = satisfyAscii f *> takeWhileAscii f
-- -- {-# INLINE takeWhileAscii1 #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyAscii :: forall chunk u e. ByteChunk chunk => (Char -> Bool) -> Parsec chunk u e Char
satisfyAscii f = Parsec \(Env# c l) (Ix# o i) s -> STR# s case l ==# i of
  1# -> Fail#
  _ -> case Chunk.unsafeIndexChar8# c i of
    c -> case c `leChar#` '\x7f'# of
      1# | f (C# c) -> Ok# (Ix# (o +# 1#) (i +# 1#)) (C# c)
      _ -> Fail#
{-# INLINE satisfyAscii #-}

-- | The predicate must not return true for chars that are not ascii.
unsafeSatisfyAscii :: forall chunk u e. ByteChunk chunk => (Char -> Bool) -> Parsec chunk u e Char
unsafeSatisfyAscii f = Parsec \(Env# c l) (Ix# o i) s -> STR# s case l ==# i of
  1# -> Fail#
  _ -> case Chunk.unsafeIndexChar8# c i of
    c | f (C# c) -> Ok# (Ix# (o +# 1#) (i +# 1#)) (C# c)
    _ -> Fail#
{-# INLINE unsafeSatisfyAscii #-}

char :: ByteChunk c => Char -> Parsec c e s ()
char = text . T.singleton
{-# INLINE char #-}

asciiChar :: (HasCallStack, ByteChunk c) => Char -> Parsec c e s ()
asciiChar c =
  if c < '\x7f'
    then unsafeAsciiChar c
    else error "Text.Metalparsec.Internal.Text: not ascii char"
{-# INLINE asciiChar #-}

unsafeAsciiChar :: ByteChunk c => Char -> Parsec c e s ()
unsafeAsciiChar (C# ch) =
  Parsec \(Env# c l) (Ix# o i) s -> STR# s case l ==# i of
    1# -> Fail#
    _ -> case Chunk.unsafeIndexChar8# c i of
      ch' -> case ch `eqChar#` ch' of
        1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) ()
        _ -> Fail#
{-# INLINE unsafeAsciiChar #-}

text :: ByteChunk chunk => Text -> Parsec chunk u e ()
text (UnsafeText# bs# off# len#) = Parsec \(Env# c l) (Ix# o i) s ->
  STR# s case i +# len# <=# l of
    1# ->
      case Chunk.unsafeCompare# bs# off# c i len# of
        0# -> Ok# (Ix# (o +# len#) (i +# len#)) ()
        _ -> Fail#
    _ -> Fail#

anyChar :: ByteChunk c => Parsec c e s Char
anyChar = Parsec \(Env# c l) (Ix# o i) s -> STR# s case i ==# l of
  1# -> Fail#
  _ -> case Chunk.unsafeIndexChar8# c i of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) (C# c1)
      _ ->
        case (i +# 1#) ==# l of
          1# -> Fail#
          _ -> case Chunk.unsafeIndexChar8# c (i +# 1#) of
            c2 -> case c1 `leChar#` '\xDF'# of
              1# -> Ok# (Ix# (o +# 2#) (i +# 2#)) (C# (Utf8.char2# c1 c2))
              _ -> case (i +# 2#) ==# l of
                1# -> Fail#
                _ -> case Chunk.unsafeIndexChar8# c (i +# 2#) of
                  c3 -> case c1 `leChar#` '\xEF'# of
                    1# -> Ok# (Ix# (o +# 3#) (i +# 3#)) (C# (Utf8.char3# c1 c2 c3))
                    _ -> case (l +# 3#) ==# l of
                      1# -> Fail#
                      _ -> case Chunk.unsafeIndexChar8# c 3# of
                        c4 -> Ok# (Ix# (o +# 4#) (i +# 4#)) (C# (Utf8.char4# c1 c2 c3 c4))
{-# INLINE anyChar #-}

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: ByteChunk c => Parsec c e s ()
anyChar_ = Parsec \(Env# c l) (Ix# o i) s ->
  STR# s case i ==# l of
    1# -> Fail#
    _ -> case Chunk.unsafeIndexChar8# c i of
      c1 ->
        case c1 `leChar#` '\x7F'# of
          1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) ()
          _ ->
            case Utf8.lengthByLeader (charToWord8 (C# c1)) of
              I# len# -> case i +# len# <# l of
                1# -> Ok# (Ix# (o +# len#) (i +# len#)) ()
                _ -> Fail#
{-# INLINE anyChar_ #-}

anyCharAscii :: ByteChunk s => Parsec s u e Char
anyCharAscii = Parsec \(Env# c l) (Ix# o i) s -> STR# s case i ==# l of
  1# -> Fail#
  _ -> case Chunk.unsafeIndexChar8# c i of
    c -> case c `leChar#` '\x7F'# of
      1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) (C# c)
      _ -> Fail#
{-# INLINE anyCharAscii #-}

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfyChar :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e Char
satisfyChar f = Parsec \e ix s -> case runParsec# (anyChar @chunk) e ix s of
  STR# s r -> STR# s case r of
    Ok# p c | f c -> Ok# p c
    _ -> Fail#
{-# INLINE satisfyChar #-}

isAsciiLetter :: Char -> Bool
isAsciiLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# INLINE isAsciiLetter #-}

isAsciiDigit :: Char -> Bool
isAsciiDigit c = '0' <= c && c <= '9'
{-# INLINE isAsciiDigit #-}

-- | Does not check if eof has been hit
-- This can also result in invalid utf8.
unsafeByte :: ByteChunk c => Word8 -> Parsec c e s ()
unsafeByte (S.W8# ch) = Parsec \(Env# c _) (Ix# o i) s ->
  STR# s case Chunk.unsafeIndexWord8# c i of
    ch' -> case ch `S.eqWord8#` ch' of
      1# -> Ok# (Ix# (o +# 1#) (i +# 1#)) ()
      _ -> Fail#
{-# INLINE unsafeByte #-}
