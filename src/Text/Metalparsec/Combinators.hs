{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Text.Metalparsec.Combinators where

import Control.Applicative qualified as Applicative
import Data.Bits ((.&.))
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified
import Data.Text.Internal qualified
import GHC.Base (unsafeChr)
import GHC.Exts
import GHC.Exts qualified as Exts
import GHC.Word
import Text.Metalparsec.Chunk (ByteChunk, Chunk)
import Text.Metalparsec.Chunk qualified as Chunk
import Text.Metalparsec.Parser
import Text.Metalparsec.Util

-- | Check that the input has at least the given number of bytes.
ensureLen :: Int -> Parser s p u e ()
ensureLen (I# len) = Parser $ \_s l p i u ->
  case len <=# l of
    1# -> Ok# p i u ()
    _ -> Fail#
{-# INLINE ensureLen #-}

-- | Unsafely read a concrete byte from the input. It's not checked that the input has
unsafeTake1 :: forall chunk p u e. Chunk chunk p => Chunk.TokenTag chunk -> Parser chunk p u e ()
unsafeTake1 t =
  Parser
    ( \s _l p i u ->
        case Chunk.unsafeIndex# (proxy# @chunk) s i of
          t' | t == Chunk.tokenTag t' -> Ok# (Chunk.nextTokenPos# t' p) (i +# 1#) u ()
          _ -> Fail#
    )
{-# INLINE unsafeTake1 #-}

take1 :: forall chunk p u e. (Chunk chunk p, Chunk.NotText chunk) => Chunk.TokenTag chunk -> Parser chunk p u e ()
take1 t = ensureLen 1 *> unsafeTake1 t
{-# INLINE take1 #-}

takeWhileAscii :: forall chunk p u e. ByteChunk chunk p => (Char -> Bool) -> Parser chunk p u e Text
takeWhileAscii f = Parser $ \s l p i u -> go i s l p i u
  where
    go i0 s l p i u = case l ==# i of
      1# -> Ok# p i u (UnsafeText# s i0 (i -# i0))
      _ -> case Chunk.unsafeIndex# (proxy# @chunk) s i of
        b
          | (b .&. 0b10000000 == 0b00000000) && f (char8 b) ->
              go i0 s l (Chunk.nextTokenCharPos b p) (i +# 1#) u
          | otherwise -> Ok# p i u (UnsafeText# s i0 (i -# i0))
{-# INLINE takeWhileAscii #-}

takeWhileAscii1 :: forall chunk p u e. ByteChunk chunk p => (Char -> Bool) -> Parser chunk p u e Text
takeWhileAscii1 f = satisfyAscii f *> takeWhileAscii f
{-# INLINE takeWhileAscii1 #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyAscii :: forall chunk p u e. ByteChunk chunk p => (Char -> Bool) -> Parser chunk p u e Char
satisfyAscii f = Parser $ \s l p i u -> case l ==# i of
  1# -> Fail#
  _ -> case W8# (indexWord8Array# s i) of
    b
      | b <= 0x7f,
        let c = unsafeChr (fromIntegral b),
        f c ->
          Ok# (Chunk.nextTokenCharPos b p) (i +# 1#) u c
      | otherwise -> Fail#
{-# INLINE satisfyAscii #-}

-- anyChar :: forall chunk p u e. ByteChunk chunk p => (Char -> Bool) -> Parser chunk p u e Char
-- anyChar f = Parser $ \s l p i u -> case i ==# l of
--   1# -> Fail#
--   _ ->

-- anyChar :: forall chunk p u e. ByteChunk chunk p => Parser chunk p u e Char
-- anyChar = Parser $ \s l p i u -> case i ==# l of
--   1# -> Fail#
--   _ -> case indexChar8# s i of
--     c1 -> case c1 `leChar#` '\x7F'# of
--       1# -> Ok# (Chunk.nextTokenCharPos) (C# c1) (plusAddr# buf 1#)
--       _ -> case eqAddr# eob (plusAddr# buf 1#) of
--         1# -> Fail#
--         _ -> case indexCharOffAddr# buf 1# of
--           c2 -> case c1 `leChar#` '\xDF'# of
--             1# ->
--               let resc =
--                     ((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#)
--                       `orI#` (ord# c2 -# 0x80#)
--                in OK# (C# (chr# resc)) (plusAddr# buf 2#)
--             _ -> case eqAddr# eob (plusAddr# buf 2#) of
--               1# -> Fail#
--               _ -> case indexCharOffAddr# buf 2# of
--                 c3 -> case c1 `leChar#` '\xEF'# of
--                   1# ->
--                     let resc =
--                           ((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#)
--                             `orI#` ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 6#)
--                             `orI#` (ord# c3 -# 0x80#)
--                      in OK# (C# (chr# resc)) (plusAddr# buf 3#)
--                   _ -> case eqAddr# eob (plusAddr# buf 3#) of
--                     1# -> Fail#
--                     _ -> case indexCharOffAddr# buf 3# of
--                       c4 ->
--                         let resc =
--                               ((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#)
--                                 `orI#` ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#)
--                                 `orI#` ((ord# c3 -# 0x80#) `uncheckedIShiftL#` 6#)
--                                 `orI#` (ord# c4 -# 0x80#)
--                          in OK# (C# (chr# resc)) (plusAddr# buf 4#)

-- | Convert a parsing failure to an error.
cut :: Parser s p u e a -> e -> Parser s p u e a
cut (Parser f) e = Parser $ \s l p i u -> case f s l p i u of
  Fail# -> Err# e
  x -> x
{-# INLINE cut #-}

-- | Convert a parsing failure to a `Maybe`. If possible, use `withOption` instead.
optional :: Parser s p u e a -> Parser s p u e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# INLINE optional #-}

-- | Convert a parsing failure to a `()`.
optional_ :: Parser s p u e a -> Parser s p u e ()
optional_ p = (() <$ p) <|> pure ()
{-# INLINE optional_ #-}

-- | CPS'd version of `optional`. This is usually more efficient, since it gets rid of the
--   extra `Maybe` allocation.
withOption :: Parser s p u e a -> (a -> Parser s p u e b) -> Parser s p u e b -> Parser s p u e b
withOption (Parser f) just (Parser nothing) = Parser $ \s l p i u -> case f s l p i u of
  Ok# p i u a -> runParser# (just a) s l p i u
  Fail# -> nothing s l p i u
  Err# e -> Err# e
{-# INLINE withOption #-}

try :: Parser s p u e a -> Parser s p u e a
try (Parser f) = Parser $ \s l p i u -> case f s l p i u of
  Err# _ -> Fail#
  x -> x
{-# INLINE try #-}

encodeCharUtf8 :: Char -> [Word8]
encodeCharUtf8 = toList . textToByteArray . T.singleton

textToByteArray :: Text -> ByteArray
textToByteArray (Data.Text.Internal.Text (Data.Text.Array.ByteArray arr) _ _) = ByteArray arr

-- | Skip a parser zero or more times.
many_ :: Parser s p u e a -> Parser s p u e ()
many_ (Parser f) = Parser go
  where
    go s l p i u = case f s l p i u of
      Ok# p i u _ -> go s l p i u
      Fail# -> Ok# p i u ()
      Err# e -> Err# e
{-# INLINE many_ #-}

-- | Skip a parser one or more times.
some_ :: Parser s p u e a -> Parser s p u e ()
some_ pa = pa >> many_ pa
{-# INLINE some_ #-}

-- | Choose between two parsers. If the first parser fails, try the second one, but if the first one
--   throws an error, propagate the error.
(<|>) :: Parser s p u e a -> Parser s p u e a -> Parser s p u e a
(<|>) = (Applicative.<|>)
{-# INLINE (<|>) #-}

infixr 6 <|>

-- | Branch on a parser: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parser s p i u a -> Parser s p i u b -> Parser s p i u b -> Parser s p i u b
branch pa pt pf = Parser $ \s l p i u -> case runParser# pa s l p i u of
  Ok# p i u _ -> runParser# pt s l p i u
  Fail# -> runParser# pf s l p i u
  Err# e -> Err# e
{-# INLINE branch #-}

-- | Succeed if the input is empty.
eof :: Parser s p u e ()
eof = Parser $ \_s l p i u -> case l ==# i of
  1# -> Ok# p i u ()
  _ -> Fail#
{-# INLINE eof #-}

runParserWithAll ::
  forall chunk p u e a.
  Chunk chunk p =>
  Parser chunk p u e a ->
  u ->
  chunk ->
  Result e (a, u, Chunk.ChunkSlice chunk)
runParserWithAll (Parser f) u s = case Chunk.toSlice# @chunk s of
  Chunk.Slice# (# s#, off#, len# #) ->
    case f s# len# (Chunk.defPos# (# #)) off# u of
      Err# e -> Err e
      Fail# -> Fail
      Ok# _p i _u a -> OK (a, u, Chunk.convertSlice# (Chunk.Slice# (# s#, i, len# #)))
{-# INLINEABLE runParserWithAll #-}

runParser :: Chunk s p => Parser s p u e a -> u -> s -> Result e (a, u)
runParser p u s = (\(x, u, _) -> (x, u)) <$> Exts.inline runParserWithAll p u s
{-# INLINEABLE runParser #-}

evalParser :: Chunk s p => Parser s p u e a -> u -> s -> Result e a
evalParser p u s = fst <$> Exts.inline runParser p u s
{-# INLINEABLE evalParser #-}

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# INLINE isLatinLetter #-}

isAsciiDigit :: Char -> Bool
isAsciiDigit c = '0' <= c && c <= '9'
{-# INLINE isAsciiDigit #-}

fail :: Parser s p u e a
fail = Parser $ \_ _ _ _ _ -> Fail#
{-# INLINE fail #-}
