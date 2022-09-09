{-# LANGUAGE CPP #-}

module Compat.Word (module X) where

#if __GLASGOW_HASKELL__ < 920
import GHC.Word.Compat as X
#else
import GHC.Word as X
#endif
