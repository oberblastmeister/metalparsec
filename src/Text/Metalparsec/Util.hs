module Text.Metalparsec.Util where

import Data.ByteString qualified as B
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified
import Data.Text.Encoding qualified as T
import Data.Text.Internal qualified
import GHC.Base (ord, unsafeChr)
import GHC.Exts
import GHC.IO (IO (..))
import Compat.Word

unI# :: Int -> Int#
unI# (I# i#) = i#
{-# INLINE unI# #-}

indexChar8# :: ByteArray# -> Int# -> Char#
indexChar8# = indexCharArray#
{-# INLINE indexChar8# #-}

indexChar8 :: ByteArray -> Int -> Char
indexChar8 (ByteArray bs#) (I# i#) = C# (indexCharArray# bs# i#)
{-# INLINE indexChar8 #-}

-- char8 :: Word8 -> Char
-- char8 = unsafeChr . fromIntegral
-- {-# INLINE char8 #-}

-- char8# :: Word8# -> Char
-- char8# w = C# (chr# (word2Int# (word8ToWord# w)))
-- {-# INLINE char8# #-}

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord
{-# INLINE charToWord8 #-}

pattern UnsafeText# :: ByteArray# -> Int# -> Int# -> Data.Text.Internal.Text
pattern UnsafeText# bs# off# len# = Data.Text.Internal.Text (Data.Text.Array.ByteArray bs#) (I# off#) (I# len#)

{-# COMPLETE UnsafeText# #-}

encodeCharUtf8 :: Char -> [Word8]
encodeCharUtf8 = toList . T.encodeUtf8 . T.singleton

charUtf8Len :: Char -> Int
charUtf8Len = textUtf8Len . T.singleton

textUtf8Bytes :: Text -> [Word8]
textUtf8Bytes = toList . T.encodeUtf8

charUtf8Bytes :: Char -> [Word8]
charUtf8Bytes = textUtf8Bytes . T.singleton

textUtf8Len :: Text -> Int
textUtf8Len = B.length . T.encodeUtf8

type Assert :: Bool -> Constraint -> Constraint
type family Assert check errMsg where
  Assert 'True _ = ()
  Assert _ errMsg = errMsg

accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE accursedUnutterablePerformIO #-}
