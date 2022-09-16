module Text.Metalparsec.Internal.Utf8
  ( lengthByLeader,
    char2#,
    char3#,
    char4#,
    textBytes,
    charBytes,
  )
where

import Text.Metalparsec.Internal.Compat.Word
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Internal.Encoding.Utf8 as T.Internal.Encoding.Utf8
import GHC.Exts
import qualified GHC.Exts as Exts
import qualified Data.ByteString as B

lengthByLeader :: Word8 -> Int
lengthByLeader w = Exts.inline T.Internal.Encoding.Utf8.utf8LengthByLeader w
{-# INLINE lengthByLeader #-}

char2# :: Char# -> Char# -> Char#
char2# c1# c2# =
  chr#
    ( ((ord# c1# -# 0xC0#) `uncheckedIShiftL#` 6#)
        `orI#` (ord# c2# -# 0x80#)
    )
{-# INLINE char2# #-}

char3# :: Char# -> Char# -> Char# -> Char#
char3# c1# c2# c3# =
  chr#
    ( ((ord# c1# -# 0xE0#) `uncheckedIShiftL#` 12#)
        `orI#` ((ord# c2# -# 0x80#) `uncheckedIShiftL#` 6#)
        `orI#` (ord# c3# -# 0x80#)
    )
{-# INLINE char3# #-}

char4# :: Char# -> Char# -> Char# -> Char# -> Char#
char4# c1# c2# c3# c4# =
  chr#
    ( ((ord# c1# -# 0xF0#) `uncheckedIShiftL#` 18#)
        `orI#` ((ord# c2# -# 0x80#) `uncheckedIShiftL#` 12#)
        `orI#` ((ord# c3# -# 0x80#) `uncheckedIShiftL#` 6#)
        `orI#` (ord# c4# -# 0x80#)
    )
{-# INLINE char4# #-}

textBytes :: Text -> [Word8]
textBytes = B.unpack . T.encodeUtf8

charBytes :: Char -> [Word8]
charBytes = textBytes . T.singleton
