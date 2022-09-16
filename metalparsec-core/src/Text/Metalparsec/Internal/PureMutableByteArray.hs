{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Text.Metalparsec.Internal.PureMutableByteArray
  ( PureMutableByteArray#,
    unsafeIndex#,
    unsafeIndexChar8#,
    fromByteString#,
    sliceByteString#,
    unsafeCompare#,
    unsafeFind#,
    unsafeIndexWord8#,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.Primitive.ByteArray (MutableByteArray (..))
import Data.Word (Word8)
import Foreign qualified
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Exts.Compat
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (PlainPtr))
import GHC.IO (IO (..))
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Word (Word8 (..))
import Text.Metalparsec.Internal.C qualified as C
import Text.Metalparsec.Internal.SizedCompat qualified as S
import Text.Metalparsec.Internal.Util (accursedUnutterablePerformIO)

newtype PureMutableByteArray# = UnsafePureMutableArray# (MutableByteArray# RealWorld)

unsafeIndex# :: PureMutableByteArray# -> Int# -> Word8
unsafeIndex# (UnsafePureMutableArray# marr) i = accursedUnutterablePerformIO $ IO $ \s -> case readWord8Array# marr i s of
  (# s, w #) -> (# s, W8# w #)
{-# INLINE unsafeIndex# #-}

unsafeIndexChar8# :: PureMutableByteArray# -> Int# -> Char#
unsafeIndexChar8# (UnsafePureMutableArray# marr) i = case readCharArray# marr i realWorld# of
  (# _, c #) -> c
{-# INLINE unsafeIndexChar8# #-}

unsafeIndexWord8# :: PureMutableByteArray# -> Int# -> Word8#
unsafeIndexWord8# (UnsafePureMutableArray# marr) i = case S.readWord8Array# marr i realWorld# of
  (# _, w #) -> w
{-# INLINE unsafeIndexWord8# #-}

fromByteString# :: ByteString -> (# PureMutableByteArray#, Int#, Int# #)
fromByteString# bs@(B.Internal.PS fp@(ForeignPtr _ fpc) o l) =
  let res = unsafeDupablePerformIO $ withForeignPtr fp $ \p -> case fpc of
        PlainPtr marr -> do
          let base = Ptr (mutableByteArrayContents# marr)
              off = p `Foreign.minusPtr` base
          pure $ (MutableByteArray marr, off, (off + l + o))
        _ -> case B.copy bs of
          B.Internal.PS fp@(ForeignPtr _ fpc) o l -> withForeignPtr fp $ \p -> case fpc of
            PlainPtr marr -> do
              let base = Ptr (mutableByteArrayContents# marr)
                  off = p `Foreign.minusPtr` base
              pure $ (MutableByteArray marr, off, (off + l + o))
            _ -> error "should be PlainPtr"
   in case res of
        (MutableByteArray marr, (I# off), (I# len)) -> (# UnsafePureMutableArray# marr, off, len #)
{-# INLINE fromByteString# #-}

sliceByteString# :: (# PureMutableByteArray#, Int#, Int# #) -> ByteString
sliceByteString# ((# (UnsafePureMutableArray# marr), off, len #)) =
  B.Internal.PS (ForeignPtr (mutableByteArrayContents# marr) (PlainPtr marr)) (I# off) (I# (len -# off))
{-# INLINE sliceByteString# #-}

unsafeCompare# :: ByteArray# -> Int# -> PureMutableByteArray# -> Int# -> Int# -> Int#
unsafeCompare# arr i1 (UnsafePureMutableArray# marr) i2 l =
  case accursedUnutterablePerformIO $
    C.memcmp_off'
      arr
      (fromIntegral (I# i1))
      marr
      (fromIntegral (I# i2))
      (fromIntegral (I# l)) of
    (fromIntegral -> (I# i)) -> i
{-# INLINE unsafeCompare# #-}

-- unsafeCompare arr i1 marr i2 l s =
--   go 0# s
--   where
--     go i s = case i ==# l of
--       1# -> (# s, 0# #)
--       _ ->
--         case indexWord8Array# arr (i +# i1) of
--           b1 -> case readWord8Array# marr (i +# i2) s of
--             (# s, b2 #) -> case b1 `eqWord8#` b2 of
--               1# -> go (i +# 1#) s
--               _ -> (# s, word2Int# (word8ToWord# (b1 `subWord8#` b2)) #)
-- {-# INLINE unsafeCompare #-}

unsafeFind# :: PureMutableByteArray# -> Int# -> Word8# -> Int#
unsafeFind# (UnsafePureMutableArray# bs) o b =
  case fromIntegral $
    accursedUnutterablePerformIO $
      C.memchr_off'
        bs
        (fromIntegral $ I# o)
        (fromIntegral $ S.W8# b)
        (fromIntegral $ I# (sizeofMutableByteArray# bs -# o)) of
    I# i -> i
{-# INLINE unsafeFind# #-}
