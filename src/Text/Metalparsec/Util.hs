module Text.Metalparsec.Util where

import Data.Primitive.ByteArray (ByteArray (..))
import Data.Text.Array qualified
import Data.Text.Internal qualified
import GHC.Base (unsafeChr)
import GHC.Exts
import GHC.Word

unI# :: Int -> Int#
unI# (I# i#) = i#
{-# INLINE unI# #-}

indexChar8# :: ByteArray# -> Int# -> Char#
indexChar8# = indexCharArray#
{-# INLINE indexChar8# #-}

indexChar8 :: ByteArray -> Int -> Char
indexChar8 (ByteArray bs#) (I# i#) = C# (indexCharArray# bs# i#)
{-# INLINE indexChar8 #-}

char8 :: Word8 -> Char
char8 = unsafeChr . fromIntegral
{-# INLINE char8 #-}

char8# :: Word8# -> Char
char8# w# = char8 (W8# w#)
{-# INLINE char8# #-}

pattern UnsafeText# :: ByteArray# -> Int# -> Int# -> Data.Text.Internal.Text
pattern UnsafeText# bs# off# len# = Data.Text.Internal.Text (Data.Text.Array.ByteArray bs#) (I# off#) (I# len#)
{-# INLINE UnsafeText# #-}

{-# COMPLETE UnsafeText# #-}
