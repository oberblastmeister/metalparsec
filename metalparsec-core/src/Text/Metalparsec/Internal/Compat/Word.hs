{-# LANGUAGE CPP #-}

module Text.Metalparsec.Internal.Compat.Word (module X) where

#if MIN_VERSION_base(4,16,0)
import GHC.Word as X
#else
import GHC.Word.Compat as X
#endif
