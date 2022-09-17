module Text.Metalparsec.Internal.Combinators
  ( ensureLen,
    cut,
    cutting,
    optional,
    optional_,
    many_,
    some_,
    (<|>),
    branch,
    eof,
    runParser,
    fail,
    slice,
    manySlice,
    lookahead,
    fails,
    notFollowedBy,

    -- * chain
    chainPre,
    chainPost,
    chainl1,
    chainr1,

    -- * @from Text.Metalparsec.Internal@
    err,
    try,
    tryWith,
    withOff#,
    withPos#,
    getPos,
    getState,
    putState,
    withOption,
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
cut (Parsec f) er = Parsec $ \e p s -> case f e p s of
  STR# s r ->
    STR# s $# case r of
      Fail# -> Err# er
      x -> x

-- | Run the parser, if we get a failure, throw the given error, but if we get an error, merge the
-- inner and the newly given errors using the @e -> e -> e@ function. This can be useful for
-- implementing parsing errors which may propagate hints or accummulate contextual information.
cutting :: Parsec c e s a -> e -> (e -> e -> e) -> Parsec c e s a
cutting (Parsec f) er merge = Parsec $ \e p s -> case f e p s of
  STR# s r ->
    STR# s $# case r of
      Fail# -> Err# er
      Err# er' -> let !er'' = merge er' er in Err# er''
      x -> x

-- | Convert a parsing failure to a `Maybe`. If possible, use `withOption` instead.
optional :: Parsec c e s a -> Parsec c e s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Convert a parsing failure to a `()`.
optional_ :: Parsec c e s a -> Parsec c e s ()
optional_ p = (() <$ p) <|> pure ()

-- | CPS'd version of `optional`. This is usually more efficient, since it gets rid of the
-- extra `Maybe` allocation.
withOption :: Parsec c e s a -> (a -> Parsec c e s b) -> Parsec c e s b -> Parsec c e s b
withOption (Parsec f) just (Parsec nothing) = Parsec $ \e p s -> case f e p s of
  STR# s r -> case r of
    Ok# p a -> runParsec# (just a) e p s
    Fail# -> nothing e p s
    Err# e -> STR# s $# Err# e
{-# INLINE withOption #-}

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

fail :: Parsec c e s a
fail = parser# $ \_ _ -> Fail#

-- | Return a slice consumed by a parser.
slice :: forall chunk u e a. Chunk chunk => Parsec chunk u e a -> Parsec chunk u e (Chunk.ChunkSlice chunk)
slice (Parsec f) = Parsec $ \e@(Env# c _) p@(Ix# _ i0) s -> case f e p s of
  STR# s r ->
    STR# s $# case r of
      Ok# p@(Ix# _ i) _a -> Ok# p (Chunk.convertSlice# @chunk (# c, i0, i -# i0 #))
      x -> unsafeCoerceRes# x

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
      STR# s $# case r of
        Ok# _ _ -> Fail#
        Fail# -> Ok# p ()
        Err# e -> Err# e

-- | Succeed if the first parser succeeds and the second one fails.
notFollowedBy :: Parsec c e s a -> Parsec c e s b -> Parsec c e s a
notFollowedBy p1 p2 = p1 <* fails p2

-- | An analogue of the list `foldr` function: parse zero or more @a@-s, terminated by a @b@, and
-- combine the results in a right-nested way using the @a -> b -> b@ function. Note: this is not
-- the usual `chainr` function from the parsec libraries!
chainPre :: Parsec c e s (a -> a) -> Parsec c e s a -> Parsec c e s a
chainPre (Parsec elem) (Parsec end) = Parsec go
  where
    go e p s = case elem e p s of
      STR# s r -> case r of
        Ok# p f -> case go e p s of
          STR# s r ->
            STR# s $# case r of
              Ok# p b -> let !b' = f b in Ok# p b'
              x -> x
        Fail# -> end e p s
        Err# e -> STR# s $# Err# e
{-# INLINE chainPre #-}

chainPost :: Parsec c e s a -> Parsec c e s (a -> a) -> Parsec c e s a
chainPost (Parsec elem) (Parsec post) = Parsec $ \e p s -> case elem e p s of
  STR# s r -> case r of
    Ok# p a -> go e p s a
    x -> STR# s x
  where
    go e p s x = case post e p s of
      STR# s r -> case r of
        Ok# p f -> let !x' = f x in go e p s x'
        Fail# -> STR# s $# Ok# p x
        Err# e -> STR# s $# Err# e
{-# INLINE chainPost #-}

-- | @chainl1 p op@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. This parser can for example be used to eliminate left
-- recursion which typically occurs in expression grammars.
--
-- >  expr    = term   `chainl1` addop
-- >  term    = factor `chainl1` mulop
-- >  factor  = parens expr <|> integer
-- >
-- >  mulop   =   do{ symbol "*"; return (*)   }
-- >          <|> do{ symbol "/"; return (div) }
-- >
-- >  addop   =   do{ symbol "+"; return (+) }
-- >          <|> do{ symbol "-"; return (-) }
chainl1 :: forall c e s a. Parsec c e s a -> Parsec c e s (a -> a -> a) -> Parsec c e s a
chainl1 (Parsec elem) (Parsec sep) = Parsec $ \e p s -> case elem e p s of
  STR# s r -> case r of
    Ok# p a -> go e p s a
    x -> STR# s x
  where
    go e p s x = case sep e p s of
      STR# s r -> case r of
        Ok# p f -> case elem e p s of
          STR# s r -> case r of
            Ok# p y -> let !x' = f x y in go e p s x'
            Fail# -> STR# s $# Ok# p x
            Err# e -> STR# s $# Err# e
        Fail# -> STR# s $# Ok# p x
        Err# e -> STR# s $# Err# e
{-# INLINE chainl1 #-}

-- | @chainr1 p op x@ parses /one/ or more occurrences of |p|,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@.
chainr1 :: Parsec c e s a -> Parsec c e s (a -> a -> a) -> Parsec c e s a
chainr1 (Parsec elem) (Parsec sep) = Parsec start
  where
    start e p s = case elem e p s of
      STR# s r ->
        case r of
          Ok# p x -> go e p s x
          x -> STR# s x

    go e p s x = case sep e p s of
      STR# s r -> case r of
        Ok# p f -> case start e p s of
          STR# s r ->
            STR# s $# case r of
              Ok# p y -> let !b' = f x y in Ok# p b'
              Fail# -> Ok# p x
              Err# e -> Err# e
        Fail# -> STR# s $# Ok# p x
        Err# e -> STR# s $# Err# e
{-# INLINE chainr1 #-}
