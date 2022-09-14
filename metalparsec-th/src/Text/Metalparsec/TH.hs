{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.TH where

import Data.Text qualified as T
import GHC.Exts
import Language.Haskell.TH (Exp, ExpQ, Q)
#if MIN_VERSION_base(4,15,0)
import Language.Haskell.TH (Code)
#else
import Language.Haskell.TH (TExp)
#endif

import Data.Word (Word8)
import Text.Metalparsec.Internal.Chunk (ByteChunk)
import Text.Metalparsec (ByteChunk, Parsec, ensureLen)
import Text.Metalparsec.Internal.Utf8 qualified as Utf8
import Text.Metalparsec.Text.Unsafe qualified as Text.Unsafe
import GHC.Base (unsafeChr)

#if MIN_VERSION_base(4,15,0)
type Up = Code Q
#else
type Up a = Q (TExp a)
#endif

string :: ByteChunk c => String -> Up (Parsec c e s ())
string = bytes . Utf8.textBytes . T.pack

string' :: String -> Q Exp
string' = bytes' . Utf8.textBytes . T.pack

char :: (ByteChunk c) => Char -> Up (Parsec c e s ())
char = bytes . Utf8.charBytes

char' :: Char -> ExpQ
char' = bytes' . Utf8.charBytes

bytes :: ByteChunk c => [Word8] -> Up (Parsec c e s ())
bytes bs = let len = length bs in [||ensureLen len *> $$(unsafeBytes bs)||]

bytes' :: [Word8] -> ExpQ
bytes' bs = let len = length bs in [|ensureLen len *> $(unsafeBytes' bs)|]

unsafeBytes ::  (ByteChunk c) => [Word8] -> Up (Parsec c e s ())
unsafeBytes = go
  where
    go (b : bs) = [||Text.Unsafe.unsafeByte (char8 b) *> $$(go bs)||]
    go [] = [||pure ()||]

unsafeBytes' :: [Word8] -> ExpQ
unsafeBytes' = go
  where
    go (b : bs) = [|Text.Unsafe.unsafeByte (char8 b) *> $(go bs)|]
    go [] = [|pure ()|]

char8 :: Word8 -> Char
char8 = unsafeChr . fromIntegral
