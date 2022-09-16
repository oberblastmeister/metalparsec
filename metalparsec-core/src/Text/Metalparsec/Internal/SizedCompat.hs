{-# LANGUAGE CPP #-}

-- | Compatibility layer for numeric primops.
--
-- GHC 9.2 standardized unboxed numeric primops. Prior, it was quite asymmetric.
-- Many primop functions used the native unboxed numerics 'Word#' and 'Int#' even
-- if a sized unboxed numeric was in the name, e.g. 'indexWord8OffAddr#' returning
-- 'Word#' pre-9.2. All boxed machine integers only stored 'Word#' internally!
--
-- We target GHC 9.2's better handling. In order to maintain compatibility with
-- older GHCs, we define missing primops and wrap ones that changed type. Usually,
-- we can write a few wrappers so that 9.2 uses sized unboxed numerics everywhere,
-- and pre-9.2 uses native unboxed numerics everywhere. Sometimes we really want to
-- work with sized unboxed numerics on both, in which case we have to do more
-- involved primop wrapping.
--
-- The general pattern is as follows:
--
--  * A ticked primop means it's sized on >=9.2, native on <9.2
--  * A double-ticked primop means it's sized on all
--  * An unticked primop should mean the same as a ticked primop (no guarantees)
--
-- Also see: https://gitlab.haskell.org/ghc/ghc/-/wikis/Unboxed-Numerics
module Text.Metalparsec.Internal.SizedCompat
  ( eqWord8#,
    wordToWord8#,
    word8ToWord#,
    indexWord8Array#,
    readWord8Array#,
    pattern W8#,
  )
where

import GHC.Exts qualified as E
import GHC.Word qualified as W

{- ORMOLU_DISABLE -}
eqWord8# :: E.Word8# -> E.Word8# -> E.Int#
wordToWord8# :: E.Word# -> E.Word8#
word8ToWord# :: E.Word8# -> E.Word#
indexWord8Array# :: E.ByteArray# -> E.Int# -> E.Word8#
readWord8Array# :: E.MutableByteArray# s -> E.Int# -> E.State# s -> (# E.State# s, E.Word8# #)
pattern W8# :: E.Word8# -> W.Word8
{-# INLINE eqWord8# #-}
{-# INLINE wordToWord8# #-}
{-# INLINE word8ToWord# #-}
{-# INLINE indexWord8Array# #-}
{-# INLINE readWord8Array# #-}
{- ORMOLU_ENABLE -}

#if MIN_VERSION_base(4,16,0)
-- GHC >=9.2

eqWord8# = E.eqWord8#
wordToWord8# = E.wordToWord8#
word8ToWord# = E.word8ToWord#
indexWord8Array# = E.indexWord8Array#
readWord8Array# = E.readWord8Array#
pattern W8# w# = W.W8# w#
{-# INLINE W8# #-}
#else
-- GHC <9.2

eqWord8# = E.eqWord8#
wordToWord8# = narrowWord8#
word8ToWord# = extendWord8#
indexWord8Array# bs# i# = E.wordToWord8# (E.indexWord8Array# bs# i#)
readWord8Array# bs# i# s# = case E.readWord8Array# bs# i# s# of
  (# s#, w# #) -> (# s#, wordToWord8# #)
pattern W8# w# = W.W8# (wordToWord8# -> w#)
  where
    W8# w# = W.W8# (word8ToWord# w#)
#endif

#if !MIN_VERSION_base(4,13,0)
-- GHC <8.8

type Word8# = Word#
narrowWord8# :: Word# -> Word8#
narrowWord8# = narrow8Word#
extendWord8# :: Word# -> Word8#
extendWord8# w# = w#
leWord8# :: Word8# -> Word8# -> Int#
leWord8# w1# w2# = leWord# w1# w2#
eqWord8# :: Word8# -> Word8# -> Int#
eqWord8# w1# w2# = eqWord# w1# w2#
#endif
