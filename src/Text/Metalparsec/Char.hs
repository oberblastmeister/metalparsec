module Text.Metalparsec.Char where

import Data.Text.Internal.Encoding.Utf8 qualified as Text.Internal.Encoding.Utf8
import GHC.Exts
import GHC.Exts qualified as Exts
import Text.Metalparsec.Chunk (ByteChunk)
import Text.Metalparsec.Chunk qualified as Chunk
import Text.Metalparsec.Combinators
import Text.Metalparsec.Internal
import Text.Metalparsec.Util

takeWhileChar :: forall chunk p u e. (ByteChunk chunk p) => (Char -> Bool) -> Parsec chunk p u e (Chunk.ChunkSlice chunk)
takeWhileChar = takeWhileSuceeds . satisfyChar
{-# INLINE takeWhileChar #-}

takeWhileChar1 :: forall chunk p u e. (ByteChunk chunk p) => (Char -> Bool) -> Parsec chunk p u e (Chunk.ChunkSlice chunk)
takeWhileChar1 f = satisfyChar f *> takeWhileChar f
{-# INLINE takeWhileChar1 #-}

takeWhileAscii :: forall chunk p u e. (ByteChunk chunk p) => (Char -> Bool) -> Parsec chunk p u e (Chunk.ChunkSlice chunk)
takeWhileAscii = takeWhileSuceeds . satisfyAscii
{-# INLINE takeWhileAscii #-}

takeWhileAscii1 :: forall chunk p u e. ByteChunk chunk p => (Char -> Bool) -> Parsec chunk p u e (Chunk.ChunkSlice chunk)
takeWhileAscii1 f = satisfyAscii f *> takeWhileAscii f
{-# INLINE takeWhileAscii1 #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyAscii :: forall chunk p u e. ByteChunk chunk p => (Char -> Bool) -> Parsec chunk p u e Char
satisfyAscii f = Parsec $ \s l p i u -> case l ==# i of
  1# -> Fail#
  _ -> case indexChar8# s i of
    c -> case c `leChar#` '\x7f'# of
      1# | f (C# c) -> Ok# (Chunk.onAscii (charToWord8 (C# c)) p) (i +# 1#) u (C# c)
      _ -> Fail#
{-# INLINE satisfyAscii #-}

anyChar :: forall chunk p u e. ByteChunk chunk p => Parsec chunk p u e Char
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
              1# ->
                let resc =
                      ((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#)
                        `orI#` (ord# c2 -# 0x80#)
                 in Ok# (Chunk.incPosChar 2# p) (i +# 2#) u (C# (chr# resc))
              _ -> case (i +# 2#) ==# l of
                1# -> Fail#
                _ -> case indexChar8# s (i +# 2#) of
                  c3 -> case c1 `leChar#` '\xEF'# of
                    1# ->
                      let resc =
                            ((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#)
                              `orI#` ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 6#)
                              `orI#` (ord# c3 -# 0x80#)
                       in Ok# (Chunk.incPosChar 3# p) (i +# 3#) u (C# (chr# resc))
                    _ -> case (l +# 3#) ==# l of
                      1# -> Fail#
                      _ -> case indexChar8# s 3# of
                        c4 ->
                          let resc =
                                ((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#)
                                  `orI#` ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#)
                                  `orI#` ((ord# c3 -# 0x80#) `uncheckedIShiftL#` 6#)
                                  `orI#` (ord# c4 -# 0x80#)
                           in Ok# (Chunk.incPosChar 4# p) (i +# 4#) u (C# (chr# resc))
{-# INLINE anyChar #-}

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: forall s p u e. ByteChunk s p => Parsec s p u e ()
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

anyCharAscii :: forall chunk p u e. ByteChunk chunk p => Parsec chunk p u e Char
anyCharAscii = Parsec $ \s l p i u -> case i ==# l of
  1# -> Fail#
  _ -> case indexChar8# s i of
    c -> case c `leChar#` '\x7F'# of
      1# -> Ok# (Chunk.onAscii (charToWord8 (C# c)) p) (i +# 1#) u (C# c)
      _ -> Fail#
{-# INLINE anyCharAscii #-}

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfyChar :: forall chunk p u e. (ByteChunk chunk p) => (Char -> Bool) -> Parsec chunk p u e Char
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
