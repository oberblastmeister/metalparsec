{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Text.Metalparsec.Token where

import qualified Control.Applicative as Applicative
import GHC.Exts
import Text.Metalparsec.Internal
import Text.Metalparsec.Internal.Chunk (Chunk)
import qualified Text.Metalparsec.Internal.Chunk as Chunk
import qualified Text.Metalparsec.Internal.Combinators as Combinators
import Prelude hiding (fail)

-- -- | Unsafely read a concrete byte from the input. It's not checked that the input has
-- unsafeScan1 :: Chunk.TokenChunk s => Chunk.TokenTag s -> Parsec c s e ()
-- unsafeScan1 t =
--   Parsec
--     ( \s _l i p u ->
--         case Chunk.unsafeIndex# s i of
--           t' | t == Chunk.tokenTag t' -> Ok# (p +# Chunk.tokenOffset# t') (i +# 1#) u ()
--           _ -> Fail#
--     )
-- {-# INLINE unsafeScan1 #-}

-- scan1 :: Chunk.TokenChunk s => Chunk.TokenTag s -> Parsec c s e ()
-- scan1 t = Combinators.ensureLen 1 *> unsafeScan1 t
-- {-# INLINE scan1 #-}

-- any :: Chunk.TokenChunk s => Parsec c s e (Chunk.Token s)
-- any = Parsec $ \e ix s -> case i ==# l of
--   1# -> Fail#
--   _ -> case Chunk.unsafeIndex# s i of
--     t -> Ok# (p +# Chunk.tokenOffset# t) (i +# 1#) u t
-- {-# INLINE any #-}

-- satisfy :: Chunk.TokenChunk s => (Chunk.TokenTag s -> Bool) -> Parsec c s e (Chunk.TokenTag s)
-- satisfy f = Parsec $ \e ix s -> case i ==# l of
--   1# -> Fail#
--   _ -> case Chunk.unsafeIndex# s i of
--     t | let t' = Chunk.tokenTag t, f t' -> Ok# i p u t'
--     _ -> Fail#
