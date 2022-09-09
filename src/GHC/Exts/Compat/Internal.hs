{-# LANGUAGE CPP #-}

module GHC.Exts.Compat.Internal where

import Unsafe.Coerce
import GHC.Exts

mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# marr# = byteArrayContents# (unsafeCoerce# marr#)
{-# INLINE mutableByteArrayContents# #-}
