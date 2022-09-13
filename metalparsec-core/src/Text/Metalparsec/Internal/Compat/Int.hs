{-# LANGUAGE CPP #-}

module Compat.Int (module X) where

#if MIN_VERSION_base(4,16,0)
import GHC.Int as X
#else
import GHC.Int.Compat as X
#endif
