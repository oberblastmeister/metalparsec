module Text.Metalparsec.Char where

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
takeWhileChar = takeWhileSuceeds . satisfyChar
{-# INLINE takeWhileChar #-}

takeWhileChar1 :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
takeWhileChar1 f = satisfyChar f *> takeWhileChar f
{-# INLINE takeWhileChar1 #-}

takeWhileAscii :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
takeWhileAscii = takeWhileSuceeds . satisfyAscii
{-# INLINE takeWhileAscii #-}

takeWhileAscii1 :: forall chunk u e. ByteChunk chunk => (Char -> Bool) -> Parsec chunk u e (Chunk.ChunkSlice chunk)
takeWhileAscii1 f = satisfyAscii f *> takeWhileAscii f
{-# INLINE takeWhileAscii1 #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyAscii :: forall chunk u e. ByteChunk chunk => (Char -> Bool) -> Parsec chunk u e Char
satisfyAscii f = Parsec $ \s l p i u -> case l ==# i of
  1# -> Fail#
  _ -> case indexChar8# s i of
    c -> case c `leChar#` '\x7f'# of
      1# | f (C# c) -> Ok# (Chunk.onAscii (charToWord8 (C# c)) p) (i +# 1#) u (C# c)
      _ -> Fail#
{-# INLINE satisfyAscii #-}

char :: Char -> Parsec s u e ()
char c = undefined

-- text :: forall chunk u e. ByteChunk chunk => Text -> Parsec chunk u e ()
-- text (UnsafeText# bs# off# len#) = Parsec start
--   where
--     diff# = len# -# off#
--     start s l p i u = case i +# diff# <=# l of
--       1# -> go s l p i u
--       _ -> Fail#
--     go s l p i u =
--       let w = Chunk.unsafeIndex# (proxy# @chunk) s i
--           utf8Len = Exts.inline T.Internal.Encoding.Utf8.utf8LengthByLeader (W8# w)
--        in case Utf8.lengthByLeader# w of
        -- 1# -> compareByteArrays#
        --   Ok# (Chunk.onAscii t p) (i +# 1#) u ()

-- case utf8Len of
--   1

anyChar :: forall chunk u e. ByteChunk chunk => Parsec chunk u e Char
anyChar = Parsec $ \s l p i u -> case i ==# l of
  1# -> Fail#
  _ -> case indexChar8# s i of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> Ok# (Chunk.onAscii (charToWord8 (C# c1)) p) (i +# 1#) u (C# c1)
      _ ->
        case (i +# 1#) ==# l of
          1# -> Fail#
          _ -> case indexChar8# s (i +# 1#) of
            c2 -> case c1 `leChar#` '\xDF'# of
              1# -> Ok# (Chunk.incPosChar 2# p) (i +# 2#) u (C# (Utf8.char2# c1 c2))
              _ -> case (i +# 2#) ==# l of
                1# -> Fail#
                _ -> case indexChar8# s (i +# 2#) of
                  c3 -> case c1 `leChar#` '\xEF'# of
                    1# -> Ok# (Chunk.incPosChar 3# p) (i +# 3#) u (C# (Utf8.char3# c1 c2 c3))
                    _ -> case (l +# 3#) ==# l of
                      1# -> Fail#
                      _ -> case indexChar8# s 3# of
                        c4 -> Ok# (Chunk.incPosChar 4# p) (i +# 4#) u (C# (Utf8.char4# c1 c2 c3 c4))
{-# INLINE anyChar #-}

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: forall s u e. ByteChunk s => Parsec s u e ()
anyChar_ = Parsec $ \s l p i u ->
  case i ==# l of
    1# -> Fail#
    _ -> case indexChar8# s i of
      c1 ->
        case c1 `leChar#` '\x7F'# of
          1# -> Ok# (Chunk.onAscii (charToWord8 (C# c1)) p) (i +# 1#) u ()
          _ ->
            case Exts.inline Text.Internal.Encoding.Utf8.utf8LengthByLeader (charToWord8 (C# c1)) of
              I# len# -> case i +# len# <# l of
                1# -> Ok# (Chunk.incPosChar len# p) (i +# len#) u ()
                _ -> Fail#
{-# INLINE anyChar_ #-}

anyCharAscii :: forall chunk u e. ByteChunk chunk => Parsec chunk u e Char
anyCharAscii = Parsec $ \s l p i u -> case i ==# l of
  1# -> Fail#
  _ -> case indexChar8# s i of
    c -> case c `leChar#` '\x7F'# of
      1# -> Ok# (Chunk.onAscii (charToWord8 (C# c)) p) (i +# 1#) u (C# c)
      _ -> Fail#
{-# INLINE anyCharAscii #-}

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfyChar :: forall chunk u e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk u e Char
satisfyChar f = Parsec $ \s l p i u -> case runParsec# (anyChar @chunk) s l p i u of
  Ok# p i u c | f c -> Ok# p i u c
  _ -> Fail#
{-# INLINE satisfyChar #-}

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# INLINE isLatinLetter #-}

isAsciiDigit :: Char -> Bool
isAsciiDigit c = '0' <= c && c <= '9'
{-# INLINE isAsciiDigit #-}
