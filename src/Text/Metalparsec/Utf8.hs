module Text.Metalparsec.Utf8
  ( lengthByLeader,
    char2#,
    char3#,
    char4#,
  )
where

import Compat.Word
import Data.Text.Internal.Encoding.Utf8 qualified as T.Internal.Encoding.Utf8
import GHC.Exts
import GHC.Exts qualified as Exts

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
