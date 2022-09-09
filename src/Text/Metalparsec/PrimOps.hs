{-# LANGUAGE CPP #-}

module Text.Metalparsec.PrimOps where

import GHC.Exts
import Unsafe.Coerce

#if __GLASGOW_HASKELL__ < 920
mutableByteArrayContents## mbarr# = byteArrayContents# (unsafeCoerceUnlifted mbarr#)
#else
mutableByteArrayContents## = mutableByteArrayContents#
#endif

