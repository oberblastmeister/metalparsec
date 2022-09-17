{-# LANGUAGE CPP #-}

-- | Compatibility layer for numeric primops.
--
-- GHC 9.2 standardized unboxed numeric primops. Prior, it was quite asymmetric.
-- Many primop functions used the native unboxed numerics 'Word#' and 'Int#' even
-- if a sized unboxed numeric was in the name, e.g. 'indexWord8OffAddr#' returning
-- 'Word#' pre-9.2. All boxed machine integers only stored 'Word#' internally!
--
-- Also see: https://gitlab.haskell.org/ghc/ghc/-/wikis/Unboxed-Numerics
module Text.Metalparsec.Internal.SizedCompat
  ( eqWord8#,
    wordToWord8#,
    word8ToWord#,
    indexWord8Array#,
    readWord8Array#,
    Word8#,
    pattern W8#,
  )
where

import qualified GHC.Exts as E
import qualified GHC.Word as W

{- ORMOLU_DISABLE -}
eqWord8# :: Word8# -> Word8# -> E.Int#
wordToWord8# :: E.Word# -> Word8#
word8ToWord# :: Word8# -> E.Word#
indexWord8Array# :: E.ByteArray# -> E.Int# -> Word8#
readWord8Array# :: E.MutableByteArray# s -> E.Int# -> E.State# s -> (# E.State# s, Word8# #)
pattern W8# :: Word8# -> W.Word8
{-# INLINE eqWord8# #-}
{-# INLINE wordToWord8# #-}
{-# INLINE word8ToWord# #-}
{-# INLINE indexWord8Array# #-}
{-# INLINE readWord8Array# #-}
{-# COMPLETE W8# #-}
{- ORMOLU_ENABLE -}

#if MIN_VERSION_base(4,16,0)
-- GHC >=9.2
type Word8# = E.Word8#

eqWord8# = E.eqWord8#
wordToWord8# = E.wordToWord8#
word8ToWord# = E.word8ToWord#
indexWord8Array# = E.indexWord8Array#
readWord8Array# = E.readWord8Array#
pattern W8# w# = W.W8# w#
{-# INLINE W8# #-}

#elif MIN_VERSION_base(4,13,0)
-- GHC <9.2
type Word8# = E.Word8#

eqWord8# = E.eqWord8#
wordToWord8# = E.narrowWord8#
word8ToWord# = E.extendWord8#
indexWord8Array# bs# i# = wordToWord8# (E.indexWord8Array# bs# i#)
readWord8Array# bs# i# s# = case E.readWord8Array# bs# i# s# of
  (# s#, w# #) -> (# s#, wordToWord8# w# #)
pattern W8# w# <- W.W8# (wordToWord8# -> w#)
  where
    W8# w# = W.W8# (word8ToWord# w#)
#else
-- GHC <8.8

type Word8# = E.Word#

narrowWord8# :: E.Word# -> Word8#
narrowWord8# = E.narrow8Word#
extendWord8# :: E.Word# -> Word8#
extendWord8# w# = w#

eqWord8# w1# w2# = E.eqWord# w1# w2#
wordToWord8# = narrowWord8#
word8ToWord# = extendWord8#
indexWord8Array# bs# i# = wordToWord8# (E.indexWord8Array# bs# i#)
readWord8Array# bs# i# s# = case E.readWord8Array# bs# i# s# of
  (# s#, w# #) -> (# s#, wordToWord8# w# #)
pattern W8# w# <- W.W8# (wordToWord8# -> w#)
  where
    W8# w# = W.W8# (word8ToWord# w#)
#endif
