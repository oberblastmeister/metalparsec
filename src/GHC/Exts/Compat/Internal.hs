{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module GHC.Exts.Compat.Internal where

import GHC.Exts
import Unsafe.Coerce

#if !MIN_VERSION_base(4,15,0)
mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# marr# = byteArrayContents# (unsafeCoerce# marr#)
{-# INLINE mutableByteArrayContents# #-}
#endif
