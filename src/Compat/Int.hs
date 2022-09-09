{-# LANGUAGE CPP #-}

module Compat.Int (module X) where

#if __GLASGOW_HASKELL__ < 920
import GHC.Int.Compat as X
#else
import GHC.Int as X
#endif
