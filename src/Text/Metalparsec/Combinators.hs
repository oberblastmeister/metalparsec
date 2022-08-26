{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Text.Metalparsec.Combinators where

import Control.Applicative qualified as Applicative
import Control.Monad.ST (ST)
import Data.Primitive.PrimArray
import GHC.Exts
import GHC.Exts qualified as Exts
import Text.Metalparsec.Chunk (Chunk)
import Text.Metalparsec.Chunk qualified as Chunk
import Text.Metalparsec.Internal
import qualified Data.Vector.Generic as V

-- | Check that the input has at least the given number of bytes.
ensureLen :: Int -> Parsec s p u e ()
ensureLen (I# len) = Parsec $ \_s l p i u ->
  case len <=# l of
    1# -> Ok# p i u ()
    _ -> Fail#
{-# INLINE ensureLen #-}

-- | Unsafely read a concrete byte from the input. It's not checked that the input has
unsafeTake1 :: forall chunk p u e. Chunk chunk p => Chunk.TokenTag chunk -> Parsec chunk p u e ()
unsafeTake1 t =
  Parsec
    ( \s _l p i u ->
        case Chunk.unsafeIndex# (proxy# @chunk) s i of
          t' | t == Chunk.tokenTag t' -> Ok# (Chunk.onToken# t' p) (i +# 1#) u ()
          _ -> Fail#
    )
{-# INLINE unsafeTake1 #-}

take1 :: forall chunk p u e. (Chunk chunk p, Chunk.NotText chunk) => Chunk.TokenTag chunk -> Parsec chunk p u e ()
take1 t = ensureLen 1 *> unsafeTake1 t
{-# INLINE take1 #-}

-- | Convert a parsing failure to an error.
cut :: Parsec s p u e a -> e -> Parsec s p u e a
cut (Parsec f) e = Parsec $ \s l p i u -> case f s l p i u of
  Fail# -> Err# e
  x -> x
{-# INLINE cut #-}

-- | Convert a parsing failure to a `Maybe`. If possible, use `withOption` instead.
optional :: Parsec s p u e a -> Parsec s p u e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# INLINE optional #-}

-- | Convert a parsing failure to a `()`.
optional_ :: Parsec s p u e a -> Parsec s p u e ()
optional_ p = (() <$ p) <|> pure ()
{-# INLINE optional_ #-}

-- | CPS'd version of `optional`. This is usually more efficient, since it gets rid of the
--   extra `Maybe` allocation.
withOption :: Parsec s p u e a -> (a -> Parsec s p u e b) -> Parsec s p u e b -> Parsec s p u e b
withOption (Parsec f) just (Parsec nothing) = Parsec $ \s l p i u -> case f s l p i u of
  Ok# p i u a -> runParsec# (just a) s l p i u
  Fail# -> nothing s l p i u
  Err# e -> Err# e
{-# INLINE withOption #-}

try :: Parsec s p u e a -> Parsec s p u e a
try (Parsec f) = Parsec $ \s l p i u -> case f s l p i u of
  Err# _ -> Fail#
  x -> x
{-# INLINE try #-}

-- textToByteArray :: Text -> ByteArray
-- textToByteArray (Data.Text.Internal.Text (Data.Text.Array.ByteArray arr) _ _) = ByteArray arr

-- | Skip a Parsec zero or more times.
many_ :: Parsec s p u e a -> Parsec s p u e ()
many_ (Parsec f) = Parsec go
  where
    go s l p i u = case f s l p i u of
      Ok# p i u _ -> go s l p i u
      Fail# -> Ok# p i u ()
      Err# e -> Err# e
{-# INLINE many_ #-}

-- | Skip a Parsec one or more times.
some_ :: Parsec s p u e a -> Parsec s p u e ()
some_ pa = pa >> many_ pa
{-# INLINE some_ #-}

-- manyVec :: V.Vector v a => Parsec s p u e a -> Parsec s p u e (v a)
-- manyVec (Parsec f) = Parsec $ \s l p i u -> do
--   let step (p, i, u) = case f s l p i u of
--               Ok# p i u a -> pure $ Yield a (p, i, u)
--               Fail# -> pure Done
--               Err# e -> 

-- | Choose between two Parsecs. If the first Parsec fails, try the second one, but if the first one
--   throws an error, propagate the error.
(<|>) :: Parsec s p u e a -> Parsec s p u e a -> Parsec s p u e a
(<|>) = (Applicative.<|>)
{-# INLINE (<|>) #-}

infixr 6 <|>

-- | Branch on a Parsec: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parsec s p i u a -> Parsec s p i u b -> Parsec s p i u b -> Parsec s p i u b
branch pa pt pf = Parsec $ \s l p i u -> case runParsec# pa s l p i u of
  Ok# p i u _ -> runParsec# pt s l p i u
  Fail# -> runParsec# pf s l p i u
  Err# e -> Err# e
{-# INLINE branch #-}

-- | Succeed if the input is empty.
eof :: Parsec s p u e ()
eof = Parsec $ \_s l p i u -> case l ==# i of
  1# -> Ok# p i u ()
  _ -> Fail#
{-# INLINE eof #-}

runParserWithAll ::
  forall chunk p u e a.
  Chunk chunk p =>
  Parsec chunk p u e a ->
  u ->
  chunk ->
  Result e (a, u, Chunk.ChunkSlice chunk)
runParserWithAll (Parsec f) u s = case Chunk.toSlice# @chunk s of
  Chunk.Slice# (# s#, off#, len# #) ->
    case f s# len# (Chunk.defIntState# (# #)) off# u of
      Err# e -> Err e
      Fail# -> Fail
      Ok# _p i _u a -> OK (a, u, Chunk.convertSlice# @chunk (Chunk.Slice# (# s#, i, len# #)))
{-# INLINEABLE runParserWithAll #-}

runParser :: Chunk s p => Parsec s p u e a -> u -> s -> Result e (a, u)
runParser p u s = (\(x, u, _) -> (x, u)) <$> Exts.inline runParserWithAll p u s
{-# INLINEABLE runParser #-}

evalParser :: Chunk s p => Parsec s p u e a -> u -> s -> Result e a
evalParser p u s = fst <$> Exts.inline runParser p u s
{-# INLINEABLE evalParser #-}

fail :: Parsec s p u e a
fail = Parsec $ \_ _ _ _ _ -> Fail#
{-# INLINE fail #-}

takeWhileSuceeds :: forall chunk p u e a. Chunk chunk p => Parsec chunk p u e a -> Parsec chunk p u e (Chunk.ChunkSlice chunk)
takeWhileSuceeds parser = withParsecOff# $ \i -> Parsec $ go i
  where
    go i0 s l p i u = case runParsec# parser s l p i u of
      Fail# -> Ok# p i u (Chunk.convertSlice# @chunk (Chunk.Slice# (# s, i0, i -# i0 #)))
      Ok# p i u _ -> go i0 s l p i u
      Err# e -> Err# e
{-# INLINE takeWhileSuceeds #-}
