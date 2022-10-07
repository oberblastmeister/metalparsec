{-# LANGUAGE UnliftedFFITypes #-}

module Text.Metalparsec.Internal.ByteStringExt
  ( fromByteString#,
    sliceByteString#,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B.Internal
import Data.Primitive (ByteArray (..))
import Data.Primitive.ByteArray (MutableByteArray (..))
import qualified Foreign
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Exts.Compat
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (PlainPtr))
import GHC.IO (IO (..))
import GHC.IO.Unsafe (unsafeDupablePerformIO)

fromByteString# :: ByteString -> (# ByteArray#, Int#, Int# #)
fromByteString# bs@(B.Internal.PS fp@(ForeignPtr _ fpc) o l) =
  let res = unsafeDupablePerformIO $ do
        (MutableByteArray marr#, off, len) <- withForeignPtr fp $ \p -> case fpc of
          PlainPtr marr -> do
            let base = Ptr (mutableByteArrayContents# marr)
                off = p `Foreign.minusPtr` base
            pure (MutableByteArray marr, off, off + l + o)
          _ -> case B.copy bs of
            B.Internal.PS fp@(ForeignPtr _ fpc) o l -> withForeignPtr fp $ \p -> case fpc of
              PlainPtr marr -> do
                let base = Ptr (mutableByteArrayContents# marr)
                    off = p `Foreign.minusPtr` base
                pure (MutableByteArray marr, off, off + l + o)
              _ -> error "should be PlainPtr"
        bs <- IO $ \s -> case unsafeFreezeByteArray# marr# s of
          (# s, arr# #) -> (# s, ByteArray arr# #)
        pure (bs, off, len)
   in case res of
        (ByteArray arr, I# off, I# len) -> (# arr, off, len #)
{-# INLINE fromByteString# #-}

-- TODO: fix this
sliceByteString# :: (# ByteArray#, Int#, Int# #) -> ByteString
sliceByteString# (# arr, off, len #) =
  B.Internal.PS (ForeignPtr (byteArrayContents# arr) (PlainPtr (unsafeCoerce# arr))) (I# off) (I# (len -# off))
{-# INLINE sliceByteString# #-}
