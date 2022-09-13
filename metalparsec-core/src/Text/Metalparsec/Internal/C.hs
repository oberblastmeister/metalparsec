{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Text.Metalparsec.Internal.C where

import Text.Metalparsec.Internal.Compat.Word
import Foreign.C.Types (CInt (..), CSize (..))
import GHC.Exts
import Text.Metalparsec.Internal.Util (accursedUnutterablePerformIO)

foreign import ccall unsafe "metalparsec_memchr_off"
  memchr_off :: ByteArray# -> CSize -> CInt -> CSize -> IO CSize

foreign import ccall unsafe "metalparsec_memchr_off"
  memchr_off' :: MutableByteArray# s -> CSize -> CInt -> CSize -> IO CSize

foreign import ccall unsafe "metalparsec_memcmp_off"
  memcmp_off' :: ByteArray# -> CSize -> MutableByteArray# s -> CSize -> CSize -> IO CInt
