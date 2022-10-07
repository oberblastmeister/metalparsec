{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Text.Metalparsec.Internal.C where

import Foreign.C.Types (CInt (..), CSize (..))
import GHC.Exts

foreign import ccall unsafe "metalparsec_memchr_off"
  memchr_off :: ByteArray# -> CSize -> CInt -> CSize -> IO CSize
