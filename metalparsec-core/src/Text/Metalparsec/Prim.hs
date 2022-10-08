module Text.Metalparsec.Prim where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Exts
import Text.Metalparsec.Internal.Chunk (ByteChunk)
import qualified Text.Metalparsec.Internal.Chunk as Chunk
import qualified Text.Metalparsec.Internal.SizedCompat as S
import qualified Text.Metalparsec.Internal.Utf8 as Utf8
import Text.Metalparsec.Internal.Util

newtype Parsec c e a = Parsec
  { runParsec# ::
      BaseEnv# c ->
      Int# ->
      Res# e a
  }

type Env# (c :: UnliftedType) = (# c, Int# #)

type BaseEnv# c = Env# (Chunk.BaseArray# c)

type Res# e a =
  (#
    (#
      -- p
      Int#,
      -- a
      a
    #) |
    (# #) |
    (# e #)
  #)

pattern Env# :: c -> Int# -> Env# c
pattern Env# s l = (# s, l #)

-- | Contains return value and a pointer to the rest of the input buffer.
pattern Ok# :: Int# -> a -> Res# e a
pattern Ok# p a = (# (# p, a #) | | #)

-- | Constructor for errors which are by default non-recoverable.
pattern Err# :: e -> Res# e a
pattern Err# e = (# | | (# e #) #)

-- | Constructor for recoverable failure.
pattern Fail# :: Res# e a
pattern Fail# = (# | (# #) | #)

{-# COMPLETE Env# #-}

{-# COMPLETE Ok#, Err#, Fail# #-}

unsafeCoerceRes# :: Res# e a -> Res# e b
unsafeCoerceRes# = unsafeCoerce#
{-# INLINE unsafeCoerceRes# #-}

instance Functor (Parsec c e) where
  fmap f (Parsec g) = Parsec $ \e p -> case g e p of
    Ok# p a -> let !b = f a in Ok# p b
    x -> unsafeCoerceRes# x

instance Applicative (Parsec c e) where
  pure a = Parsec $ \_e p -> Ok# p a

  Parsec ff <*> Parsec fa = Parsec $ \e p -> case ff e p of
    Ok# p f -> case fa e p of
      Ok# p a -> let !b = f a in Ok# p b
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x

  Parsec fa <* Parsec fb = Parsec $ \e p -> case fa e p of
    Ok# p a ->
      case fb e p of
        Ok# p _ -> Ok# p a
        x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x

  Parsec fa *> Parsec fb = Parsec $ \e p -> case fa e p of
    Ok# p _ -> case fb e p of
      Ok# p b -> Ok# p b
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x

instance Monad (Parsec c e) where
  return = pure
  {-# INLINE return #-}

  Parsec fa >>= f = Parsec $ \e p -> case fa e p of
    Ok# p a -> runParsec# (f a) e p
    x -> unsafeCoerceRes# x

  (>>) = (*>)
  {-# INLINE (>>) #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyAscii :: ByteChunk c => (Char -> Bool) -> Parsec c e Char
satisfyAscii f = Parsec $ \(Env# c l) i ->
  case l ==# i of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c -> case c `leChar#` '\x7f'# of
        1# | f (C# c) -> Ok# (i +# 1#) (C# c)
        _ -> Fail#
{-# INLINE satisfyAscii #-}

-- | The predicate must not return true for chars that are not ascii.
unsafeSatisfyAscii :: ByteChunk c => (Char -> Bool) -> Parsec c e Char
unsafeSatisfyAscii f = Parsec $ \(Env# c l) i ->
  case l ==# i of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c | f (C# c) -> Ok# (i +# 1#) (C# c)
      _ -> Fail#
{-# INLINE unsafeSatisfyAscii #-}

char :: ByteChunk c => Char -> Parsec c e ()
char = text . T.singleton

unsafeAsciiChar :: ByteChunk c => Char -> Parsec c e ()
unsafeAsciiChar (C# ch) =
  Parsec $ \(Env# c l) i ->
    case l ==# i of
      1# -> Fail#
      _ -> case indexCharArray# c i of
        ch' -> case ch `eqChar#` ch' of
          1# -> Ok# (i +# 1#) ()
          _ -> Fail#

text :: ByteChunk c => Text -> Parsec c e ()
text (UnsafeText# bs# off# len#) = Parsec $ \(Env# c l) i ->
  case i +# len# <=# l of
    1# ->
      case compareByteArrays# bs# off# c i len# of
        0# -> Ok# (i +# len#) ()
        _ -> Fail#
    _ -> Fail#

-- | Parse any UTF-8-encoded `Char`.
anyChar :: ByteChunk c => Parsec c e Char
anyChar = Parsec $ \(Env# c l) i ->
  case i ==# l of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c1 -> case c1 `leChar#` '\x7F'# of
        1# -> Ok# (i +# 1#) (C# c1)
        _ ->
          case (i +# 1#) ==# l of
            1# -> Fail#
            _ -> case indexCharArray# c (i +# 1#) of
              c2 -> case c1 `leChar#` '\xDF'# of
                1# -> Ok# (i +# 2#) (C# (Utf8.char2# c1 c2))
                _ -> case (i +# 2#) ==# l of
                  1# -> Fail#
                  _ -> case indexCharArray# c (i +# 2#) of
                    c3 -> case c1 `leChar#` '\xEF'# of
                      1# -> Ok# (i +# 3#) (C# (Utf8.char3# c1 c2 c3))
                      _ -> case (l +# 3#) ==# l of
                        1# -> Fail#
                        _ -> case indexCharArray# c 3# of
                          c4 -> Ok# (i +# 4#) (C# (Utf8.char4# c1 c2 c3 c4))

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: ByteChunk c => Parsec c e ()
anyChar_ = Parsec $ \(Env# c l) i ->
  case i ==# l of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c1 ->
        case c1 `leChar#` '\x7F'# of
          1# -> Ok# (i +# 1#) ()
          _ ->
            case Utf8.lengthByLeader (charToWord8 (C# c1)) of
              I# len# -> case i +# len# <# l of
                1# -> Ok# (i +# len#) ()
                _ -> Fail#

-- | Parse any `Char` in the ASCII range, fail if the next input character is not in the range.
-- This is more efficient than `anyChar` if we are only working with ASCII.
anyCharAscii :: ByteChunk s => Parsec s e Char
anyCharAscii = Parsec $ \(Env# c l) i ->
  case i ==# l of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c -> case c `leChar#` '\x7F'# of
        1# -> Ok# (i +# 1#) (C# c)
        _ -> Fail#

-- | Skip any `Char` in the ASCII range. More efficient than `anyChar_` if we're working only with
-- ASCII.
anyCharAscii_ :: ByteChunk c => Parsec c e ()
anyCharAscii_ = void anyCharAscii

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfy :: forall chunk e. (ByteChunk chunk) => (Char -> Bool) -> Parsec chunk e Char
satisfy f = Parsec $ \e p -> case runParsec# (anyChar @chunk) e p of
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
  Parsec c e Char
fusedSatisfy f1 f2 f3 f4 = Parsec $ \(Env# c l) i ->
  case i ==# l of
    1# -> Fail#
    _ -> case indexCharArray# c i of
      c1 -> case c1 `leChar#` '\x7f'# of
        1#
          | f1 (C# c1) -> Ok# (i +# 1#) (C# c1)
          | otherwise -> Fail#
        _ -> case i +# 1# ==# l of
          1# -> Fail#
          _ -> case indexCharArray# c (i +# 1#) of
            c2 -> case c1 `leChar#` '\xdf'# of
              1#
                | let ch = C# (Utf8.char2# c1 c2), f2 ch -> Ok# (i +# 2#) ch
                | otherwise -> Fail#
              _ -> case i +# 2# ==# l of
                1# -> Fail#
                _ -> case indexCharArray# c (i +# 2#) of
                  c3 -> case c1 `leChar#` '\xef'# of
                    1#
                      | let ch = C# (Utf8.char3# c1 c2 c3), f3 ch -> Ok# (i +# 3#) ch
                      | otherwise -> Fail#
                    _ -> case indexCharArray# c (i +# 3#) of
                      c4
                        | let ch = C# (Utf8.char4# c1 c2 c3 c4), f4 ch -> Ok# (i +# 4#) ch
                        | otherwise -> Fail#
{-# INLINE fusedSatisfy #-}

-- | Does not check if eof has been hit
-- This can also result in invalid utf8.
unsafeByte :: ByteChunk c => Word8 -> Parsec c e ()
unsafeByte (S.W8# ch) = Parsec $ \(Env# c _) i ->
  ( case S.indexWord8Array# c i of
      ch' -> case ch `S.eqWord8#` ch' of
        1# -> Ok# (i +# 1#) ()
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

readInt :: ByteChunk c => Parsec c e Int
readInt = Parsec $ \(Env# c l) i ->
  case readInt# (# c, l #) i of
    (# (# #) | #) -> Fail#
    (# | (# n, i' #) #) -> Ok# i' (I# n)
