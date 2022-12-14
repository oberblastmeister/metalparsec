{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.TH where

import qualified Data.Text as T
import Language.Haskell.TH (Exp, ExpQ, Q)
#if MIN_VERSION_base(4,15,0)
import Language.Haskell.TH (Code)
#else
import Language.Haskell.TH (TExp)
#endif

import Data.Word (Word8)
import Text.Metalparsec.Internal.Chunk (ByteChunk)
import Text.Metalparsec (Parsec, ensureLen)
import qualified Text.Metalparsec.Internal.Utf8 as Utf8
import qualified Text.Metalparsec.Text.Unsafe as Text.Unsafe

#if MIN_VERSION_base(4,15,0)
type Up = Code Q
#else
type Up a = Q (TExp a)
#endif

string :: ByteChunk c => String -> Up (Parsec c s e ())
string = bytes . Utf8.textBytes . T.pack

string' :: String -> Q Exp
string' = bytes' . Utf8.textBytes . T.pack

char :: (ByteChunk c) => Char -> Up (Parsec c s e ())
char = bytes . Utf8.charBytes

char' :: Char -> ExpQ
char' = bytes' . Utf8.charBytes

bytes :: ByteChunk c => [Word8] -> Up (Parsec c s e ())
bytes bs = let len = length bs in [||ensureLen len *> $$(unsafeBytes bs)||]

bytes' :: [Word8] -> ExpQ
bytes' bs = let len = length bs in [|ensureLen len *> $(unsafeBytes' bs)|]

unsafeBytes ::  (ByteChunk c) => [Word8] -> Up (Parsec c s e ())
unsafeBytes = go
  where
    go (b : bs) = [||Text.Unsafe.unsafeByte b *> $$(go bs)||]
    go [] = [||pure ()||]

unsafeBytes' :: [Word8] -> ExpQ
unsafeBytes' = go
  where
    go (b : bs) = [|Text.Unsafe.unsafeByte b *> $(go bs)|]
    go [] = [|pure ()|]
