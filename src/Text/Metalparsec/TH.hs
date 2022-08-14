{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.TH where

import Data.Primitive.ByteArray qualified as ByteArray
import Data.Text qualified as T
import GHC.Exts
import GHC.Word
import Language.Haskell.TH (Code, Exp, Q)
import Language.Haskell.TH.Syntax qualified as TH
import Text.Metalparsec.Chunk (ByteChunk, Chunk)
import Text.Metalparsec.Chunk qualified as Chunk
import Text.Metalparsec.Combinators
import Text.Metalparsec.Parser

string :: forall s p u e. (ByteChunk s p) => String -> Code Q (Parser s p u e ())
string s =
  [||
  let p = ensureLen len *> $$(go bss)
   in p
  ||]
  where
    len = ByteArray.sizeofByteArray $ textToByteArray $ T.pack s
    bss = encodeCharUtf8 <$> s
    go (bs : bss) = [||$$(unsafeCharBytes bs) *> $$(go bss)||]
    go [] = [||pure ()||]

string' :: String -> Q Exp
string' s = [|ensureLen len *> $(go bss)|]
  where
    len = ByteArray.sizeofByteArray $ textToByteArray $ T.pack s
    bss = encodeCharUtf8 <$> s
    go (bs : bss) = [|$(unsafeCharBytes' bs) *> $(go bss)|]
    go [] = [|pure ()|]

char :: forall s p u e. (ByteChunk s p) => Char -> Code Q (Parser s p u e ())
char c = [||ensureLen len *> $$(unsafeCharBytes bs)||]
  where
    len = length bs
    bs = toList . textToByteArray . T.singleton $ c

unsafeCharBytes :: forall s p u e. (ByteChunk s p) => [Word8] -> Code Q (Parser s p u e ())
unsafeCharBytes bs =
  [||
  case $$(unsafeScanListUnchecked @Word8 bs) of
    (p :: Parser chunk p u e a) ->
      p *> (Parser $ \_ _ p i u -> OK# (Chunk.nextCharPos (proxy# :: Proxy# chunk) p) i u ())
  ||]

unsafeCharBytes' :: [Word8] -> Q Exp
unsafeCharBytes' bs =
  [|
    case $(unsafeScanListUnchecked' bs) of
      (p :: Parser chunk p u e a) ->
        p *> (Parser $ \_ _ p i u -> OK# (Chunk.nextCharPos (proxy# :: Proxy# chunk) p) i u ())
    |]

unsafeScanListUnchecked :: (TH.Lift a, Chunk.Token s ~ a, Chunk s p) => [a] -> Code Q (Parser s p u e ())
unsafeScanListUnchecked = go
  where
    go (x : xs) = [||unsafeScan1 x *> $$(go xs)||]
    go [] = [||pure ()||]

unsafeScanListUnchecked' :: TH.Lift a => [a] -> Q Exp
unsafeScanListUnchecked' = go
  where
    go (x : xs) = [|unsafeScan1 x *> $(go xs)|]
    go [] = [|pure ()|]

-- -- | Beware of alignment!
-- unsafeScanPrim :: (Prim a, Eq a) => a -> Parser ByteArray# p u e ()
-- unsafeScanPrim t = Parser $ \s l p i u ->
--   case PrimArray.indexPrimArray (PrimArray s) (I# i) of
--     t' | t == t' -> OK# p (i +# (sizeOf# t)) u ()
--     _ -> Fail#
-- {-# INLINE unsafeScanPrim #-}

-- unsafeScan16 :: Word16 -> Parser ByteArray# s u e ()
-- unsafeScan16 = unsafeScanPrim
-- {-# INLINE unsafeScan16 #-}

-- -- | Unsafely read two concrete bytes from the input. It's not checked that the input has
-- --   enough bytes.
-- scan16# :: Word16 -> Parser s p u e ()
-- scan16# (W16# c) = Parser $ \fp !r eob s n ->
--   case indexWord16OffAddr# s 0# of
--     c' -> case eqWord16'# c c' of
--       1# -> OK# () (plusAddr# s 2#) n
--       _ -> Fail#
-- {-# INLINE scan16# #-}

-- -- | Unsafely read four concrete bytes from the input. It's not checked that the input has
-- --   enough bytes.
-- scan32# :: Word32 -> Parser s p u e ()
-- scan32# (W32# c) = Parser $ \fp !r eob s n ->
--   case indexWord32OffAddr# s 0# of
--     c' -> case eqWord32'# c c' of
--       1# -> OK# () (plusAddr# s 4#) n
--       _ -> Fail#
-- {-# INLINE scan32# #-}

-- -- | Unsafely read eight concrete bytes from the input. It's not checked that the input has
-- --   enough bytes.
-- scan64# :: Word -> Parser s p u e ()
-- scan64# (W# c) = Parser $ \fp !r eob s n ->
--   case indexWord64OffAddr# s 0# of
--     c' -> case eqWord# c c' of
--       1# -> OK# () (plusAddr# s 8#) n
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
