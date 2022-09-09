module Text.Metalparsec.Char where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal.Encoding.Utf8 qualified as Text.Internal.Encoding.Utf8
import GHC.Exts
import GHC.Exts qualified as Exts
import Text.Metalparsec.Chunk (ByteChunk)
import Text.Metalparsec.Chunk qualified as Chunk
import Text.Metalparsec.Combinators
import Text.Metalparsec.Internal
import Text.Metalparsec.Utf8 qualified as Utf8
import Text.Metalparsec.Util

takeWhileChar :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
takeWhileChar = manySlice . satisfyChar
{-# INLINE takeWhileChar #-}

-- -- this is wrong
-- takeWhileChar1 :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
-- takeWhileChar1 f = satisfyChar f *> takeWhileChar f
-- {-# INLINE takeWhileChar1 #-}

takeWhileAscii :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
takeWhileAscii = manySlice . satisfyAscii
{-# INLINE takeWhileAscii #-}

-- takeWhileAscii1 :: forall chunk u e. ByteChunk chunk => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
-- takeWhileAscii1 f = satisfyAscii f *> takeWhileAscii f
-- {-# INLINE takeWhileAscii1 #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyAscii :: forall chunk u e. ByteChunk chunk => (Char -> Bool) -> Parsec chunk u e Char
satisfyAscii f = Parsec $ \s l i p u -> case l ==# i of
  1# -> Fail#
  _ -> case Chunk.unsafeIndexChar8# s i of
    c -> case c `leChar#` '\x7f'# of
      1# | f (C# c) -> Ok# (p +# 1#) (i +# 1#) u (C# c)
      _ -> Fail#
{-# INLINE satisfyAscii #-}

char :: ByteChunk s => Char -> Parsec s u e ()
char = text . T.singleton
{-# INLINE char #-}

text :: ByteChunk chunk => Text -> Parsec chunk u e ()
text (UnsafeText# bs# off# len#) = Parsec $ \s l i p u ->
  case i +# len# <=# l of
    1# ->
      case Chunk.unsafeCompare# bs# off# s i len# of
        0# -> Ok# (p +# len#) (i +# len#) u ()
        _ -> Fail#
    _ -> Fail#
{-# INLINE text #-}

anyChar :: forall chunk u e. ByteChunk chunk => Parsec chunk u e Char
anyChar = Parsec $ \s l i p u -> case i ==# l of
  1# -> Fail#
  _ -> case Chunk.unsafeIndexChar8# s i of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> Ok# (p +# 1#) (i +# 1#) u (C# c1)
      _ ->
        case (i +# 1#) ==# l of
          1# -> Fail#
          _ -> case Chunk.unsafeIndexChar8# s (i +# 1#) of
            c2 -> case c1 `leChar#` '\xDF'# of
              1# -> Ok# (p +# 2#) (i +# 2#) u (C# (Utf8.char2# c1 c2))
              _ -> case (i +# 2#) ==# l of
                1# -> Fail#
                _ -> case Chunk.unsafeIndexChar8# s (i +# 2#) of
                  c3 -> case c1 `leChar#` '\xEF'# of
                    1# -> Ok# (p +# 3#) (i +# 3#) u (C# (Utf8.char3# c1 c2 c3))
                    _ -> case (l +# 3#) ==# l of
                      1# -> Fail#
                      _ -> case Chunk.unsafeIndexChar8# s 3# of
                        c4 -> Ok# (p +# 4#) (i +# 4#) u (C# (Utf8.char4# c1 c2 c3 c4))
{-# INLINE anyChar #-}

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: forall s u e. ByteChunk s => Parsec s u e ()
anyChar_ = Parsec $ \s l i p u ->
  case i ==# l of
    1# -> Fail#
    _ -> case Chunk.unsafeIndexChar8# s i of
      c1 ->
        case c1 `leChar#` '\x7F'# of
          1# -> Ok# (p +# 1#) (i +# 1#) u ()
          _ ->
            case Utf8.lengthByLeader (charToWord8 (C# c1)) of
              I# len# -> case i +# len# <# l of
                1# -> Ok# (p +# len#) (i +# len#) u ()
                _ -> Fail#
{-# INLINE anyChar_ #-}

anyCharAscii :: forall chunk u e. ByteChunk chunk => Parsec chunk u e Char
anyCharAscii = Parsec $ \s l i p u -> case i ==# l of
  1# -> Fail#
  _ -> case Chunk.unsafeIndexChar8# s i of
    c -> case c `leChar#` '\x7F'# of
      1# -> Ok# (p +# 1#) (i +# 1#) u (C# c)
      _ -> Fail#
{-# INLINE anyCharAscii #-}

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfyChar :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e Char
satisfyChar f = Parsec $ \s l i p u -> case runParsec# (anyChar @chunk) s l i p u of
  Ok# p i u c | f c -> Ok# p i u c
  _ -> Fail#
{-# INLINE satisfyChar #-}

isAsciiLetter :: Char -> Bool
isAsciiLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# INLINE isAsciiLetter #-}

isAsciiDigit :: Char -> Bool
isAsciiDigit c = '0' <= c && c <= '9'
{-# INLINE isAsciiDigit #-}
