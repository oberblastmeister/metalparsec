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

#if MIN_VERSION_base(4,15,0)
type Up = Code Q
#else
type Up a = Q (TExp a)
#endif

string :: ByteChunk s => String -> Up (Parsec s u e ())
string = bytes . Utf8.textBytes . T.pack

string' :: String -> Q Exp
string' = bytes' . Utf8.textBytes . T.pack

char :: forall s u e. (ByteChunk s) => Char -> Up (Parsec s u e ())
char = bytes . Utf8.charBytes

char' :: Char -> ExpQ
char' = bytes' . Utf8.charBytes

bytes :: ByteChunk s => [Word8] -> Up (Parsec s u e ())
bytes bs = let len = length bs in [||ensureLen len *> $$(unsafeBytes bs)||]

bytes' :: [Word8] -> ExpQ
bytes' bs = let len = length bs in [|ensureLen len *> $(unsafeBytes' bs)|]

unsafeBytes :: forall s u e. (ByteChunk s) => [Word8] -> Up (Parsec s u e ())
unsafeBytes = go
  where
    go (b : bs) = [||Text.Unsafe.unsafeByte b *> $$(go bs)||]
    go [] = [||pure ()||]

unsafeBytes' :: [Word8] -> ExpQ
unsafeBytes' = go
  where
    go (b : bs) = [|Text.Unsafe.unsafeByte b *> $(go bs)|]
    go [] = [|pure ()|]
