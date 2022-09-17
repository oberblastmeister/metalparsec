module Text.Metalparsec.Internal.Combinators
  ( ensureLen,
    cut,
    optional,
    optional_,
    many_,
    some_,
    (<|>),
    branch,
    eof,
    runParserRest,
    runParser,
    fail,
    slice,
    manySlice,
    lookahead,
    fails,
    notFollowedBy,
    -- -- * chain
    -- chainPre,
    -- chainPost,
    -- chainl1,
    -- chainr1,

    -- -- * @from Text.Metalparsec.Internal@
    -- err,
    -- try,
    -- tryWith,
    -- withOff#,
    -- withPos#,
    -- getPos,
    -- setInt#,
    -- getState,
    -- putState,
  )
where

import qualified Control.Applicative as Applicative
import GHC.Exts
import Text.Metalparsec.Internal
import Text.Metalparsec.Internal.Chunk (Chunk)
import qualified Text.Metalparsec.Internal.Chunk as Chunk
import Prelude hiding (fail)

-- | Check that the input has at least the given number of bytes.
ensureLen :: Int -> Parsec c e s ()
ensureLen (I# len) = Parsec $ \(Env# _ l) p@(Ix# _ i) s ->
  STR#
    s
    ( case i +# len <=# l of
        1# -> Ok# p ()
        _ -> Fail#
    )
{-# INLINE ensureLen #-}

-- | Convert a parsing failure to an error.
cut :: Parsec c e s a -> e -> Parsec c e s a
cut (Parsec f) er = Parsec $ \e ix s -> case f e ix s of
  STR# s r ->
    STR# s $# case r of
      Fail# -> Err# er
      x -> x

-- | Convert a parsing failure to a `Maybe`. If possible, use `withOption` instead.
optional :: Parsec c e s a -> Parsec c e s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Convert a parsing failure to a `()`.
optional_ :: Parsec c e s a -> Parsec c e s ()
optional_ p = (() <$ p) <|> pure ()

-- -- -- | CPS'd version of `optional`. This is usually more efficient, since it gets rid of the
-- -- --   extra `Maybe` allocation.
-- -- withOption :: Parsec c e s a -> (a -> Parsec c e s b) -> Parsec c e s b -> Parsec c e s b
-- -- withOption (Parsec f) just (Parsec nothing) = Parsec $ \e ix s -> case f e ix s of
-- --   Ok# p i u a -> runParsec# (just a) e ix s
-- --   Fail# -> nothing e ix s
-- --   Err# e -> Err# e
-- -- {-# INLINE withOption #-}

-- | Skip a Parsec zero more times.
many_ :: Parsec c e s a -> Parsec c e s ()
many_ (Parsec f) = Parsec go
  where
    go e p s = case f e p s of
      STR# s r -> case r of
        Ok# p _ -> go e p s
        Fail# -> STR# s (Ok# p ())
        Err# e -> STR# s (Err# e)
{-# INLINE many_ #-}

-- | Skip a Parsec one more times.
some_ :: Parsec c e s a -> Parsec c e s ()
some_ pa = pa >> many_ pa
{-# INLINE some_ #-}

-- | Choose between two Parsecs. If the first Parsec fails, try the second one, but if the first one
--   throws an error, propagate the error.
(<|>) :: Parsec c e s a -> Parsec c e s a -> Parsec c e s a
(<|>) = (Applicative.<|>)
{-# INLINE (<|>) #-}

infixr 3 <|>

-- | Branch on a Parsec: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parsec s i u a -> Parsec s i u b -> Parsec s i u b -> Parsec s i u b
branch pa pt pf = Parsec $ \e p s -> case runParsec# pa e p s of
  STR# s r -> case r of
    Ok# p _ -> runParsec# pt e p s
    Fail# -> runParsec# pf e p s
    Err# e -> STR# s (Err# e)

-- | Succeed if the input is empty.
eof :: Parsec c e s ()
eof = parser# $ \(Env# _ l) p@(Ix# _ i) -> case l ==# i of
  1# -> Ok# p ()
  _ -> Fail#

-- evalParser :: Chunk s => Parsec c e s a -> u -> s -> Result e a
-- evalParser p u s = fst <$> runParser p u s

fail :: Parsec c e s a
fail = parser# $ \_ _ -> Fail#

-- takeWhileSuceeds :: forall chunk u e a. Chunk chunk => Parsec chunk u e a -> Parsec chunk u e (Chunk.ChunkSlice chunk)
-- takeWhileSuceeds parser = withOff# $ \i -> Parsec $ go i
--   where
--     go i0 e ix s = case runParsec# parser e ix s of
--       STR# s r -> case r of
--         Fail# -> Ok# p (Chunk.convertSlice# @chunk (Chunk.Slice# (# s, i0, i -# i0 #)))
--         Ok# p _ -> go i0 e ix s
--         Err# e -> Err# e
-- {-# INLINE takeWhileSuceeds #-}

slice :: forall chunk u e a. Chunk chunk => Parsec chunk u e a -> Parsec chunk u e (Chunk.ChunkSlice chunk)
slice (Parsec f) = Parsec $ \e@(Env# c _) p@(Ix# _ i0) s -> case f e p s of
  STR# s r ->
    STR#
      s
      ( case r of
          Ok# p@(Ix# _ i) _a -> Ok# p (Chunk.convertSlice# @chunk (# c, i0, i -# i0 #))
          x -> unsafeCoerceRes# x
      )

manySlice :: Chunk c => Parsec c e s a -> Parsec c e s (Chunk.ChunkSlice c)
manySlice = slice . many_
{-# INLINE manySlice #-}

-- | Save the parsing state, then run a parser, then restore the state.
lookahead :: Parsec c e s a -> Parsec c e s a
lookahead (Parsec f) = Parsec $ \e p s ->
  case f e p s of
    STR# s r -> STR# s (case r of Ok# _ a -> Ok# p a; x -> x)

-- | Convert a parsing failure to a success.
fails :: Parsec c e s a -> Parsec c e s ()
fails (Parsec f) = Parsec $ \e p s ->
  case f e p s of
    STR# s r ->
      STR#
        s
        ( case r of
            Ok# _ _ -> Fail#
            Fail# -> Ok# p ()
            Err# e -> Err# e
        )

-- | Succeed if the first parser succeeds and the second one fails.
notFollowedBy :: Parsec c e s a -> Parsec c e s b -> Parsec c e s a
notFollowedBy p1 p2 = p1 <* fails p2

-- | An analogue of the list `foldl` function: first parse a @b@, then parse zero or more @a@-s,
--   and combine the results in a left-nested way by the @b -> a -> b@ function. Note: this is not
--   the usual `chainl` function from the parsec libraries!
chainl :: (b -> a -> b) -> Parsec c e s b -> Parsec c e s a -> Parsec c e s b
chainl f start elem = start >>= go
  where
    go b = do { !a <- elem; go $! f b a } <|> pure b
{-# INLINE chainl #-}

-- | An analogue of the list `foldl` function: first parse a @b@, then parse zero or more @a@-s,
--   and combine the results in a left-nested way by the @b -> a -> b@ function. Note: this is not
--   the usual `chainl` function from the parsec libraries!
chainPost :: (b -> a -> b) -> Parsec c e s b -> Parsec c e s a -> Parsec c e s b
chainPost f start elem = start >>= go
  where
    go b = do { !a <- elem; go $! f b a } <|> pure b
{-# INLINE chainPost #-}

-- -- | An analogue of the list `foldr` function: parse zero or more @a@-s, terminated by a @b@, and
-- --   combine the results in a right-nested way using the @a -> b -> b@ function. Note: this is not
-- --   the usual `chainr` function from the parsec libraries!
-- chainPre :: (a -> b -> b) -> Parsec c e s a -> Parsec c e s b -> Parsec c e s b
-- chainPre f (Parsec elem) (Parsec end) = Parsec go
--   where
--     go e ix s = case elem e ix s of
--       Ok# p i u a -> case go e ix s of
--         Ok# p i u b -> let !b' = f a b in Ok# p i u b'
--         x -> x
--       Fail# -> end e ix s
--       Err# e -> Err# e
-- {-# INLINE chainPre #-}

-- chainl1 :: Parsec c e s a -> Parsec c e s (a -> a -> a) -> Parsec c e s a
-- chainl1 (Parsec elem) (Parsec sep) = Parsec $ \e ix s -> case elem e ix s of
--   Ok# p i u a -> go e ix s a
--   x -> x
--   where
--     go e ix s x = case sep e ix s of
--       Ok# p i u f -> case elem e ix s of
--         Ok# p i u y -> go e ix s $! f x y
--         Fail# -> Ok# p i u x
--         Err# e -> Err# e
--       Fail# -> Ok# p i u x
--       Err# e -> Err# e
-- {-# INLINE chainl1 #-}

-- chainr1 :: Parsec c e s a -> Parsec c e s (a -> a -> a) -> Parsec c e s a
-- chainr1 (Parsec elem) (Parsec sep) = Parsec start
--   where
--     start e ix s = case elem e ix s of
--       Ok# p i u x -> go e ix s x
--       x -> x

--     go e ix s x = case sep e ix s of
--       Ok# p i u f -> case start e ix s of
--         Ok# p i u y -> let !b' = f x y in Ok# p i u b'
--         Fail# -> Ok# p i u x
--         Err# e -> Err# e
--       Fail# -> Ok# p i u x
--       Err# e -> Err# e
-- {-# INLINE chainr1 #-}
