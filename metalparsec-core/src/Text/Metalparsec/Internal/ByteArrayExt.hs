module Text.Metalparsec.Internal.ByteArrayExt
  ( unsafeFind#,
  )
where

import GHC.Exts
import qualified Text.Metalparsec.Internal.C as C
import qualified Text.Metalparsec.Internal.SizedCompat as S
import Text.Metalparsec.Internal.Util (accursedUnutterablePerformIO)

unsafeFind# :: ByteArray# -> Int# -> S.Word8# -> Int#
unsafeFind# bs o b =
  case fromIntegral $
    accursedUnutterablePerformIO $
      C.memchr_off
        bs
        (fromIntegral $ I# o)
        (fromIntegral $ S.W8# b)
        (fromIntegral $ I# (sizeofByteArray# bs -# o)) of
    I# i -> i
{-# INLINE unsafeFind# #-}
