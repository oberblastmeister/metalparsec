{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Text.Metalparsec.Combinators where

import Control.Applicative qualified as Applicative
-- import Control.Monad.ST (ST)
-- import Data.Primitive.PrimArray
-- import Data.Vector.Generic qualified as V
import GHC.Exts
import GHC.Exts qualified as Exts
import Text.Metalparsec.Chunk (Chunk)
import Text.Metalparsec.Chunk qualified as Chunk
import Text.Metalparsec.Internal

-- | Check that the input has at least the given number of bytes.
ensureLen :: Int -> Parsec s u e ()
ensureLen (I# len) = Parsec $ \_s l i p u ->
  case i +# len <=# l of
    1# -> Ok# p i u ()
    _ -> Fail#
{-# INLINE ensureLen #-}

-- | Unsafely read a concrete byte from the input. It's not checked that the input has
unsafeTake1 :: forall chunk u e. Chunk.TokenChunk chunk => Chunk.TokenTag chunk -> Parsec chunk u e ()
unsafeTake1 t =
  Parsec
    ( \s _l i p u ->
        case Chunk.unsafeIndex# s i of
          t' | t == Chunk.tokenTag t' -> Ok# (p +# Chunk.tokenOffset# t') (i +# 1#) u ()
          _ -> Fail#
    )
{-# INLINE unsafeTake1 #-}

take1 :: forall chunk u e. (Chunk.TokenChunk chunk, Chunk.NotText chunk) => Chunk.TokenTag chunk -> Parsec chunk u e ()
take1 t = ensureLen 1 *> unsafeTake1 t
{-# INLINE take1 #-}

-- | Convert a parsing failure to an error.
cut :: Parsec s u e a -> e -> Parsec s u e a
cut (Parsec f) e = Parsec $ \s l i p u -> case f s l i p u of
  Fail# -> Err# e
  x -> x
{-# INLINE cut #-}

-- | Convert a parsing failure to a `Maybe`. If possible, use `withOption` instead.
optional :: Parsec s u e a -> Parsec s u e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# INLINE optional #-}

-- | Convert a parsing failure to a `()`.
optional_ :: Parsec s u e a -> Parsec s u e ()
optional_ p = (() <$ p) <|> pure ()
{-# INLINE optional_ #-}

-- | CPS'd version of `optional`. This is usually more efficient, since it gets rid of the
--   extra `Maybe` allocation.
withOption :: Parsec s u e a -> (a -> Parsec s u e b) -> Parsec s u e b -> Parsec s u e b
withOption (Parsec f) just (Parsec nothing) = Parsec $ \s l i p u -> case f s l i p u of
  Ok# p i u a -> runParsec# (just a) s l i p u
  Fail# -> nothing s l i p u
  Err# e -> Err# e
{-# INLINE withOption #-}

try :: Parsec s u e a -> Parsec s u e a
try (Parsec f) = Parsec $ \s l i p u -> case f s l i p u of
  Err# _ -> Fail#
  x -> x
{-# INLINE try #-}

-- | Skip a Parsec zero more times.
many_ :: Parsec s u e a -> Parsec s u e ()
many_ (Parsec f) = Parsec go
  where
    go s l i p u = case f s l i p u of
      Ok# p i u _ -> go s l i p u
      Fail# -> Ok# p i u ()
      Err# e -> Err# e
{-# INLINE many_ #-}

-- | Skip a Parsec one more times.
some_ :: Parsec s u e a -> Parsec s u e ()
some_ pa = pa >> many_ pa
{-# INLINE some_ #-}

-- manyVec :: V.Vector v a => Parsec s u e a -> Parsec s u e (v a)
-- manyVec (Parsec f) = Parsec $ \s l i p u -> do
--   let step (p, i, u) = case f s l i p u of
--               Ok# p i u a -> pure $ Yield a (p, i, u)
--               Fail# -> pure Done
--               Err# e ->

-- | Choose between two Parsecs. If the first Parsec fails, try the second one, but if the first one
--   throws an error, propagate the error.
(<|>) :: Parsec s u e a -> Parsec s u e a -> Parsec s u e a
(<|>) = (Applicative.<|>)
{-# INLINE (<|>) #-}

infixr 3 <|>

-- | Branch on a Parsec: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parsec s i u a -> Parsec s i u b -> Parsec s i u b -> Parsec s i u b
branch pa pt pf = Parsec $ \s l i p u -> case runParsec# pa s l i p u of
  Ok# p i u _ -> runParsec# pt s l i p u
  Fail# -> runParsec# pf s l i p u
  Err# e -> Err# e
{-# INLINE branch #-}

-- | Succeed if the input is empty.
eof :: Parsec s u e ()
eof = Parsec $ \_s l i p u -> case l ==# i of
  1# -> Ok# p i u ()
  _ -> Fail#
{-# INLINE eof #-}

runParserWithAll ::
  forall chunk u e a.
  Chunk chunk =>
  Parsec chunk u e a ->
  u ->
  chunk ->
  Result e (a, u, Chunk.ChunkSlice chunk)
runParserWithAll (Parsec f) u s = case Chunk.toSlice# @chunk s of
  Chunk.Slice# (# s#, off#, len# #) ->
    case f s# len# off# 0# u of
      Err# e -> Err e
      Fail# -> Fail
      Ok# _p i _u a -> OK (a, u, Chunk.convertSlice# @chunk (Chunk.Slice# (# s#, i, len# #)))
{-# INLINEABLE runParserWithAll #-}

runParser :: Chunk s => Parsec s u e a -> u -> s -> Result e (a, u)
runParser p u s = (\(x, u, _) -> (x, u)) <$> Exts.inline runParserWithAll p u s
{-# INLINEABLE runParser #-}

evalParser :: Chunk s => Parsec s u e a -> u -> s -> Result e a
evalParser p u s = fst <$> Exts.inline runParser p u s
{-# INLINEABLE evalParser #-}


fail :: Parsec s u e a
fail = Parsec $ \_ _ _ _ _ -> Fail#
{-# INLINE fail #-}

takeWhileSuceeds :: forall chunk u e a. Chunk chunk => Parsec chunk u e a -> Parsec chunk u e (Chunk.ChunkSlice chunk)
takeWhileSuceeds parser = withParsecOff# $ \i -> Parsec $ go i
  where
    go i0 s l i p u = case runParsec# parser s l i p u of
      Fail# -> Ok# p i u (Chunk.convertSlice# @chunk (Chunk.Slice# (# s, i0, i -# i0 #)))
      Ok# p i u _ -> go i0 s l i p u
      Err# e -> Err# e
{-# INLINE takeWhileSuceeds #-}

slice :: forall chunk u e a. Chunk chunk => Parsec chunk u e a -> Parsec chunk u e (Chunk.ChunkSlice chunk)
slice (Parsec f) = Parsec $ \s l i0 p u -> case f s l i0 p u of
  Ok# p i u _a -> Ok# p i u (Chunk.convertSlice# @chunk (Chunk.Slice# (# s, i0, i -# i0 #)))
  x -> unsafeCoerceRes# x
{-# INLINE slice #-}

manySlice :: Chunk s => Parsec s u e a -> Parsec s u e (Chunk.ChunkSlice s)
manySlice = slice . many_
{-# INLINE manySlice #-}

-- | Save the parsing state, then run a parser, then restore the state.
lookahead :: Parsec s u e a -> Parsec s u e a
lookahead (Parsec f) = Parsec $ \s l i p u ->
  case f s l i p u of
    Ok# _ _ _ a -> Ok# p i u a
    x -> x
{-# INLINE lookahead #-}

-- | Convert a parsing failure to a success.
fails :: Parsec s u e a -> Parsec s u e ()
fails (Parsec f) = Parsec $ \s l i p u ->
  case f s l i p u of
    Ok# _ _ _ _ -> Fail#
    Fail# -> Ok# p i u ()
    Err# e -> Err# e
{-# INLINE fails #-}

-- | Succeed if the first parser succeeds and the second one fails.
notFollowedBy :: Parsec s u e a -> Parsec s u e b -> Parsec s u e a
notFollowedBy p1 p2 = p1 <* fails p2
{-# INLINE notFollowedBy #-}

-- | Throw a parsing error. By default, parser choice `(<|>)` can't backtrack
--   on parser error. Use `try` to convert an error to a recoverable failure.
err :: e -> Parsec s u e a
err e = Parsec $ \_ _ _ _ _ -> Err# e
{-# INLINE err #-}
