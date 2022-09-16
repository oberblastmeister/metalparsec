module Text.Metalparsec.Internal.ByteArrayExt
  ( unsafeFind#,
  )
where

import GHC.Exts
import qualified Text.Metalparsec.Internal.C as C
import Text.Metalparsec.Internal.SizedCompat
import Text.Metalparsec.Internal.Util (accursedUnutterablePerformIO)

unsafeFind# :: ByteArray# -> Int# -> Word8# -> Int#
unsafeFind# bs o b =
  case fromIntegral $
    accursedUnutterablePerformIO $
      C.memchr_off
        bs
        (fromIntegral $ I# o)
        (fromIntegral $ W8# b)
        (fromIntegral $ I# (sizeofByteArray# bs -# o)) of
    I# i -> i
{-# INLINE unsafeFind# #-}
