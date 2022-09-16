{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module GHC.Exts.Compat.Internal where

import GHC.Exts
import Unsafe.Coerce

#if !MIN_VERSION_base(4,16,0)
type UnliftedType = TYPE 'UnliftedRep

mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# marr# = byteArrayContents# (unsafeCoerce# marr#)
{-# INLINE mutableByteArrayContents# #-}
#endif

