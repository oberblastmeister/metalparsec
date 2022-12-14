
module Text.Metalparsec.Internal.Util
  ( charToWord8,
    pattern UnsafeText#,
    accursedUnutterablePerformIO,
  )
where

import Text.Metalparsec.Internal.Compat.Word
import qualified Data.Text.Array
import qualified Data.Text.Internal
import GHC.Base (ord)
import GHC.Exts
import GHC.IO (IO (..))

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord
{-# INLINE charToWord8 #-}

pattern UnsafeText# :: ByteArray# -> Int# -> Int# -> Data.Text.Internal.Text
pattern UnsafeText# bs# off# len# = Data.Text.Internal.Text (Data.Text.Array.ByteArray bs#) (I# off#) (I# len#)

{-# COMPLETE UnsafeText# #-}

accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE accursedUnutterablePerformIO #-}
