{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Text.Metalparsec.TH where

import Data.Text qualified as T
import GHC.Exts
import Compat.Word
import Language.Haskell.TH (Exp, ExpQ, Q)
#if MIN_VERSION_base(4,15,0)
import Language.Haskell.TH (Code)
#else
import Language.Haskell.TH (TExp)
#endif

import Text.Metalparsec.Chunk (ByteChunk)
import Text.Metalparsec.Chunk qualified as Chunk
import Text.Metalparsec.Combinators
import Text.Metalparsec.Internal
import Text.Metalparsec.Util
import Text.Metalparsec.UnboxedNumerics

#if MIN_VERSION_base(4,15,0)
type Up = Code Q
#else
type Up a = Q (TExp a)
#endif

string :: ByteChunk s => String -> Up (Parsec s u e ())
string = bytes . textUtf8Bytes . T.pack

string' :: String -> Q Exp
string' = bytes' . textUtf8Bytes . T.pack

char :: forall s u e. (ByteChunk s) => Char -> Up (Parsec s u e ())
char = bytes . charUtf8Bytes

bytes :: ByteChunk s => [Word8] -> Up (Parsec s u e ())
bytes bs = let len = length bs in [||ensureLen len *> $$(unsafeBytes bs)||]

bytes' :: [Word8] -> ExpQ
bytes' bs = let len = length bs in [|ensureLen len *> $(unsafeBytes' bs)|]

unsafeBytes :: forall s u e. (ByteChunk s) => [Word8] -> Up (Parsec s u e ())
unsafeBytes = go
  where
    go (b : bs) = [||unsafeByte b *> $$(go bs)||]
    go [] = [||pure ()||]

unsafeBytes' :: [Word8] -> ExpQ
unsafeBytes' = go
  where
    go (b : bs) = [|unsafeByte b *> $(go bs)|]
    go [] = [|pure ()|]

unsafeByte :: ByteChunk s => Word8 -> Parsec s u e ()
unsafeByte (W8# w) = Parsec $ \s _l i p u -> case Chunk.unsafeIndex# s i of
  (W8# w') -> case w `eqWord8##` w' of
    1# -> Ok# (p +# 1#) (i +# 1#) u ()
    _ -> Fail#

-- [||
-- case $$(unsafeScanListUnchecked @Word8 bs) of
--   (p :: Parsec chunk u e a) ->
--     p *> (Parsec $ \_ _ p i u -> Ok# (Chunk.onChar# 1# p) i u ())
-- \||]

-- bytes' :: [Word8] -> Q Exp
-- bytes' bs =
--   [|
--     case $(unsafeScanListUnchecked' @Word8 bs) of
--       (p :: Parsec chunk u e a) ->
--         p *> (Parsec $ \_ _ p i u -> Ok# (Chunk.onChar# 1# p) i u ())
--     |]

-- unsafeScanListUnchecked :: (TH.Lift a, Chunk.TokenTag s ~ a, Chunk s) => [a] -> Up (Parsec s u e ())
-- unsafeScanListUnchecked = go
--   where
--     go (x : xs) = [||unsafeTake1 x *> $$(go xs)||]
--     go [] = [||pure ()||]

-- unsafeByte :: ByteChunk s => Word8 -> Up (Parsec s u e ())
-- unsafeByte t =
--   [||
--   Parsec $ \s _l i p u ->
--     case Chunk.unsafeIndex# s i of
--       t' | t == t' -> Ok# (p +# 1#) (i +# 1#) u ()
--       _ -> Fail#
--   ||]

-- unsafeByte' :: ByteChunk s => Word8 -> Parsec s u e ()
-- unsafeByte' t =
--   Parsec $ \s _l i p u ->
--     case Chunk.unsafeIndex# s i of
--       t' | t == t' -> Ok# (p +# 1#) (i +# 1#) u ()
--       _ -> Fail#

-- unsafeByte' :: Word8 -> Int -> ExpQ
-- unsafeByte' t at =
--   [|
--     Parsec $ \(s :: chunk) _l i p u ->
--       case Chunk.unsafeIndex# (proxy# @chunk) s at of
--         t' | t == t' -> Ok# p (i +# 1#) u ()
--         _ -> Fail#
-- \|]

-- -- unsafeScanListUnchecked' :: TH.Lift a => [a] -> Q Exp
-- unsafeScanListUnchecked' = go
--   where
--     go (x : xs) = [|unsafeTake1 x *> $(go xs)|]
--     go [] = [|pure ()|]

-- -- | Beware of alignment!
-- unsafeScanPrim :: (Prim a, Eq a) => a -> Parsec ByteArray# p u e ()
-- unsafeScanPrim t = Parsec $ \s l p i u ->
--   case PrimArray.indexPrimArray (PrimArray s) (I# i) of
--     t' | t == t' -> Ok# p (i +# (sizeOf# t)) u ()
--     _ -> Fail#
-- {-# INLINE unsafeScanPrim #-}

-- unsafeScan16 :: Word16 -> Parsec ByteArray# s u e ()
-- unsafeScan16 = unsafeScanPrim
-- {-# INLINE unsafeScan16 #-}

-- -- | Unsafely read two concrete bytes from the input. It's not checked that the input has
-- --   enough bytes.
-- scan16# :: Word16 -> Parsec s u e ()
-- scan16# (W16# c) = Parsec $ \fp !r eob s n ->
--   case indexWord16OffAddr# s 0# of
--     c' -> case eqWord16'# c c' of
--       1# -> Ok# () (plusAddr# s 2#) n
--       _ -> Fail#
-- {-# INLINE scan16# #-}

-- -- | Unsafely read four concrete bytes from the input. It's not checked that the input has
-- --   enough bytes.
-- scan32# :: Word32 -> Parsec s u e ()
-- scan32# (W32# c) = Parsec $ \fp !r eob s n ->
--   case indexWord32OffAddr# s 0# of
--     c' -> case eqWord32'# c c' of
--       1# -> Ok# () (plusAddr# s 4#) n
--       _ -> Fail#
-- {-# INLINE scan32# #-}

-- -- | Unsafely read eight concrete bytes from the input. It's not checked that the input has
-- --   enough bytes.
-- scan64# :: Word -> Parsec s u e ()
-- scan64# (W# c) = Parsec $ \fp !r eob s n ->
--   case indexWord64OffAddr# s 0# of
--     c' -> case eqWord# c c' of
--       1# -> Ok# () (plusAddr# s 8#) n
--       _ -> Fail#
-- {-# INLINE scan64# #-}

-- splitBytes :: [Word] -> ([Word], [Word])
-- splitBytes ws = case quotRem (length ws) 8 of
--   (0, _) -> (ws, [])
--   (_, r) -> (as, chunk8s bs)
--     where
--       (as, bs) = splitAt r ws
--       chunk8s [] = []
--       chunk8s ws =
--         let (as, bs) = splitAt 8 ws
--          in packBytes as : chunk8s bs

-- packBytes :: [Word] -> Word
-- packBytes = fst . foldl' go (0, 0)
--   where
--     go (acc, shift) w | shift == 64 = error "packWords: too many bytes"
--     go (acc, shift) w = (unsafeShiftL (fromIntegral w) shift .|. acc, shift + 8)

-- let !(leading, w8s) = splitBytes bytes
--     !scanw8s = go w8s
--       where
--         go (w8 : []) = [|scan64# w8|]
--         go (w8 : w8s) = [|scan64# w8 >> $(go w8s)|]
--         go [] = [|pure ()|]
-- case w8s of
--   [] -> go leading
--     where
--       go (a : b : c : d : []) = let !w = packBytes [a, b, c, d] in [|scan32# w|]
--       go (a : b : c : d : ws) = let !w = packBytes [a, b, c, d] in [|scan32# w >> $(go ws)|]
--       go (a : b : []) = let !w = packBytes [a, b] in [|scan16# w|]
--       go (a : b : ws) = let !w = packBytes [a, b] in [|scan16# w >> $(go ws)|]
--       go (a : []) = [|scan8# a|]
--       go [] = [|pure ()|]
--   _ -> case leading of
--     [] -> scanw8s
--     [a] -> [|scan8# a >> $scanw8s|]
--     ws@[a, b] -> let !w = packBytes ws in [|scan16# w >> $scanw8s|]
--     ws@[a, b, c, d] -> let !w = packBytes ws in [|scan32# w >> $scanw8s|]
--     ws ->
--       let !w = packBytes ws
--           !l = length ws
--        in [|scanPartial64# l w >> $scanw8s|]
