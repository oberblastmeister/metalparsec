{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Text.Metalparsec.PureMutableByteArray
  ( PureMutableByteArray#,
    unsafeIndex#,
    unsafeIndexChar8#,
    fromByteString#,
    sliceByteString#,
    unsafeCompare#,
  )
where

import GHC.Exts.Compat
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.Primitive.ByteArray (MutableByteArray (..))
import Data.Word (Word8)
import Foreign qualified
import Foreign.C.Types (CInt (..), CSize (..))
import GHC.ForeignPtr
  ( 
    ForeignPtr (..),
    ForeignPtrContents (PlainPtr),
  )
import Foreign.ForeignPtr (
  withForeignPtr)
import GHC.IO (IO (..))
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Int (Int32 (..))
import GHC.Word (Word8 (..))
import Text.Metalparsec.Util (accursedUnutterablePerformIO)

newtype PureMutableByteArray# = UnsafePureMutableArray# (MutableByteArray# RealWorld)

unsafeIndex# :: PureMutableByteArray# -> Int# -> Word8
unsafeIndex# (UnsafePureMutableArray# marr) i = accursedUnutterablePerformIO $ IO $ \s -> case readWord8Array# marr i s of
  (# s, w #) -> (# s, W8# w #)
{-# INLINE unsafeIndex# #-}

unsafeIndexChar8# :: PureMutableByteArray# -> Int# -> Char#
unsafeIndexChar8# (UnsafePureMutableArray# marr) i = case readCharArray# marr i realWorld# of
  (# _, c #) -> c
{-# INLINE unsafeIndexChar8# #-}

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
unsafeCompare# arr i1 (UnsafePureMutableArray# marr) i2 l = case accursedUnutterablePerformIO $
  c_metalparsec_memcmp_off
    arr
    (fromIntegral (I# i1))
    marr
    (fromIntegral (I# i2))
    (fromIntegral (I# l)) of
  (fromIntegral -> (I# i)) -> i
{-# INLINE unsafeCompare# #-}

-- compareByteArrayMutableByteArray arr i1 marr i2 l s =
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
-- {-# INLINE compareByteArrayMutableByteArray #-}

foreign import ccall unsafe "metalparsec_memcmp_off"
  c_metalparsec_memcmp_off :: ByteArray# -> CSize -> MutableByteArray# s -> CSize -> CSize -> IO CInt
