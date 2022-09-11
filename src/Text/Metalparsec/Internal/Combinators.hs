{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Text.Metalparsec.Internal.Combinators
  ( ensureLen,
    unsafeTake1,
    take1,
    cut,
    optional,
    optional_,
    withOption,
    many_,
    some_,
    (<|>),
    branch,
    eof,
    runParserWithAll,
    runParser,
    evalParser,
    fail,
    takeWhileSuceeds,
    slice,
    manySlice,
    lookahead,
    fails,
    notFollowedBy,
    -- * @from Text.Metalparsec.Internal@
    err,
    try,
    tryWith,
    withOff#,
    withPos#,
    getPos,
    setInt#,
    getState,
    putState,
  )
where

import Control.Applicative qualified as Applicative
import GHC.Exts
import Text.Metalparsec.Internal
import Text.Metalparsec.Internal.Chunk (Chunk)
import Text.Metalparsec.Internal.Chunk qualified as Chunk
import Prelude hiding (fail)

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

-- | Convert a parsing failure to a `Maybe`. If possible, use `withOption` instead.
optional :: Parsec s u e a -> Parsec s u e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Convert a parsing failure to a `()`.
optional_ :: Parsec s u e a -> Parsec s u e ()
optional_ p = (() <$ p) <|> pure ()

-- | CPS'd version of `optional`. This is usually more efficient, since it gets rid of the
--   extra `Maybe` allocation.
withOption :: Parsec s u e a -> (a -> Parsec s u e b) -> Parsec s u e b -> Parsec s u e b
withOption (Parsec f) just (Parsec nothing) = Parsec $ \s l i p u -> case f s l i p u of
  Ok# p i u a -> runParsec# (just a) s l i p u
  Fail# -> nothing s l i p u
  Err# e -> Err# e
{-# INLINE withOption #-}

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

-- | Succeed if the input is empty.
eof :: Parsec s u e ()
eof = Parsec $ \_s l i p u -> case l ==# i of
  1# -> Ok# p i u ()
  _ -> Fail#

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

runParser :: Chunk s => Parsec s u e a -> u -> s -> Result e (a, u)
runParser p u s = (\(x, u, _) -> (x, u)) <$> runParserWithAll p u s

evalParser :: Chunk s => Parsec s u e a -> u -> s -> Result e a
evalParser p u s = fst <$> runParser p u s

fail :: Parsec s u e a
fail = Parsec $ \_ _ _ _ _ -> Fail#

takeWhileSuceeds :: forall chunk u e a. Chunk chunk => Parsec chunk u e a -> Parsec chunk u e (Chunk.ChunkSlice chunk)
takeWhileSuceeds parser = withOff# $ \i -> Parsec $ go i
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

manySlice :: Chunk s => Parsec s u e a -> Parsec s u e (Chunk.ChunkSlice s)
manySlice = slice . many_
{-# INLINE manySlice #-}

-- | Save the parsing state, then run a parser, then restore the state.
lookahead :: Parsec s u e a -> Parsec s u e a
lookahead (Parsec f) = Parsec $ \s l i p u ->
  case f s l i p u of
    Ok# _ _ _ a -> Ok# p i u a
    x -> x

-- | Convert a parsing failure to a success.
fails :: Parsec s u e a -> Parsec s u e ()
fails (Parsec f) = Parsec $ \s l i p u ->
  case f s l i p u of
    Ok# _ _ _ _ -> Fail#
    Fail# -> Ok# p i u ()
    Err# e -> Err# e

-- | Succeed if the first parser succeeds and the second one fails.
notFollowedBy :: Parsec s u e a -> Parsec s u e b -> Parsec s u e a
notFollowedBy p1 p2 = p1 <* fails p2

-- | An analogue of the list `foldl` function: first parse a @b@, then parse zero or more @a@-s,
--   and combine the results in a left-nested way by the @b -> a -> b@ function. Note: this is not
--   the usual `chainl` function from the parsec libraries!
chainl :: (b -> a -> b) -> Parsec s u e b -> Parsec s u e a -> Parsec s u e b
chainl f start elem = start >>= go
  where
    go b = do { !a <- elem; go $! f b a } <|> pure b
{-# INLINE chainl #-}

chainlSep :: Parsec s u e a -> (a -> a -> a) -> Parsec s u e b -> Parsec s u e a
chainlSep elem f sep = chainl f elem $ sep *> elem
{-# INLINE chainlSep #-}

-- | An analogue of the list `foldr` function: parse zero or more @a@-s, terminated by a @b@, and
--   combine the results in a right-nested way using the @a -> b -> b@ function. Note: this is not
--   the usual `chainr` function from the parsec libraries!
chainr :: (a -> b -> b) -> Parsec s u e a -> Parsec s u e b -> Parsec s u e b
chainr f (Parsec elem) (Parsec end) = Parsec go
  where
    go s l i p u = case elem s l i p u of
      Ok# p i u a -> case go s l i p u of
        Ok# p i u b -> let !b' = f a b in Ok# p i u b'
        x -> x
      Fail# -> end s l i p u
      Err# e -> Err# e
{-# INLINE chainr #-}

chainrSep :: Parsec s u e a -> (a -> a -> a) -> Parsec s u e b -> Parsec s u e a
chainrSep elem f sep = chainr f elem $ sep *> elem
{-# INLINE chainrSep #-}

chainPre :: Parsec s u e a -> (a -> a) -> Parsec s u e b -> Parsec s u e a
chainPre elem f pre = chainr (const f) pre elem
{-# INLINE chainPre #-}

chainPost :: Parsec s u e a -> (a -> a) -> Parsec s u e b -> Parsec s u e a
chainPost elem f post = chainl (\b _ -> f b) elem post
{-# INLINE chainPost #-}
