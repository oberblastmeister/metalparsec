{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Internal where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData (..))
import Control.Monad (MonadPlus)
import qualified Control.Monad as Monad
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..), bisequenceA)
import GHC.Exts.Compat
import Text.Metalparsec.Internal.Chunk (Chunk)
import qualified Text.Metalparsec.Internal.Chunk as Chunk
import Prelude hiding (fail)

newtype Parsec c e s a = Parsec
  { runParsec# ::
      Env# (Chunk.BaseArray# c) ->
      Ix# ->
      s ->
      (# s, (Res# e a) #)
  }

type Res# e a =
  (#
    (#
      -- p
      Ix#,
      -- a
      a
    #) |
    (# #) |
    (# e #)
  #)

type Env# (s :: UnliftedType) = (# s, Int# #)

-- o, i
type Ix# = (# Int#, Int# #)

type STR# s e a = (# s, Res# e a #)

pattern STR# :: s -> Res# e a -> STR# s e a
pattern STR# s x = (# s, x #)

pattern Env# :: s -> Int# -> Env# s
pattern Env# s l = (# s, l #)

pattern Ix# :: Int# -> Int# -> Ix#
pattern Ix# i# i## = (# i#, i## #)

type ST# s a = State# s -> (# State# s, a #)

-- | Contains return value and a pointer to the rest of the input buffer.
pattern Ok# :: Ix# -> a -> Res# e a
pattern Ok# ix a = (# (# ix, a #) | | #)

-- | Constructor for errors which are by default non-recoverable.
pattern Err# :: e -> Res# e a
pattern Err# e = (# | | (# e #) #)

-- | Constructor for recoverable failure.
pattern Fail# :: Res# e a
pattern Fail# = (# | (# #) | #)

{-# COMPLETE STR# #-}

{-# COMPLETE Ix# #-}

{-# COMPLETE Env# #-}

{-# COMPLETE Ok#, Err#, Fail# #-}

unsafeCoerceRes# :: Res# e a -> Res# e b
unsafeCoerceRes# = unsafeCoerce#
{-# INLINE unsafeCoerceRes# #-}

($#) :: (Res# e a -> STR# s e a) -> Res# e a -> STR# s e a
f $# r = f r
{-# INLINE ($#) #-}

instance Functor (Parsec c e s) where
  fmap f (Parsec g) = Parsec $ \e ix s -> case g e ix s of
    STR# s r ->
      STR# s $# case r of
        Ok# p a -> let !b = f a in Ok# p b
        x -> unsafeCoerceRes# x

instance Applicative (Parsec c e s) where
  pure a = Parsec $ \_e p s -> STR# s (Ok# p a)

  Parsec ff <*> Parsec fa = Parsec $ \e p s -> case ff e p s of
    STR# s r -> case r of
      Ok# p f -> case fa e p s of
        STR# s r ->
          STR# s $# case r of
            Ok# p a -> let !b = f a in Ok# p b
            x -> unsafeCoerceRes# x
      x -> STR# s (unsafeCoerceRes# x)

  Parsec fa <* Parsec fb = Parsec $ \e ix s -> case fa e ix s of
    STR# s r -> case r of
      Ok# _p a ->
        case fb e ix s of
          STR# s r ->
            STR# s $# case r of
              Ok# p _ -> Ok# p a
              x -> unsafeCoerceRes# x
      x -> STR# s (unsafeCoerceRes# x)

  Parsec fa *> Parsec fb = Parsec $ \e p s -> case fa e p s of
    STR# s r -> case r of
      Ok# p _ -> case fb e p s of
        STR# s r ->
          STR# s $# case r of
            Ok# p b -> Ok# p b
            x -> unsafeCoerceRes# x
      x -> STR# s (unsafeCoerceRes# x)

instance Monad (Parsec c e s) where
  return = pure
  {-# INLINE return #-}

  Parsec fa >>= f = Parsec $ \e p s -> case fa e p s of
    STR# s r -> case r of
      Ok# p a -> runParsec# (f a) e p s
      x -> STR# s (unsafeCoerceRes# x)

  (>>) = (*>)
  {-# INLINE (>>) #-}

-- instance Bifunctor (Parsec s u) where
--   bimap f g (Parsec m) = Parsec $ \e ix s -> case m e ix s of
--     Ok# p i u a -> Ok# p i u (g a)
--     Fail# -> Fail#
--     Err# e -> Err# (f e)

-- instance Semigroup a => Semigroup (Parsec c e s a) where
--   (<>) = liftA2 (<>)
--   {-# INLINE (<>) #-}

-- instance Monoid a => Monoid (Parsec c e s a) where
--   mempty = pure mempty

instance Alternative (Parsec c e s) where
  empty = Parsec $ \_ _ s -> STR# s Fail#

  -- \| Don't use this! @<|>@ is left associative, which is slower.
  Parsec f <|> Parsec g = Parsec $ \e ix s ->
    case f e ix s of
      STR# s r -> case r of
        Fail# -> g e ix s
        x -> STR# s x

  -- \| Run a Parsec zero more times, collect the results in a list. Note: for optimal performance,
  --   try to avoid this. Often it is possible to get rid of the intermediate list by using a
  --   combinator or a custom Parsec.
  many (Parsec f) = Parsec (go [])
    where
      go xs e p s = case f e p s of
        STR# s r -> case r of
          Ok# p x -> go (x : xs) e p s
          Fail# -> STR# s (Ok# p $ reverse xs)
          Err# e -> STR# s (Err# e)

  -- \| Run a Parsec one more times, collect the results in a list. Note: for optimal performance,
  --   try to avoid this. Often it is possible to get rid of the intermediate list by using a
  --   combinator or a custom Parsec.
  -- some p = (:) <$> p <*> many p
  some p@(Parsec f) = p >>= \x -> Parsec (go [x])
    where
      go xs e p s = case f e p s of
        STR# s r -> case r of
          Ok# p x -> go (x : xs) e p s
          Fail# -> STR# s (Ok# p $ reverse xs)
          Err# e -> STR# s (Err# e)

instance MonadPlus (Parsec c e s) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadState s (Parsec c e s) where
  get = getState
  put = putState
  {-# INLINE get #-}
  {-# INLINE put #-}

instance MonadError e (Parsec c e s) where
  throwError = err
  catchError = tryWith
  {-# INLINE throwError #-}
  {-# INLINE catchError #-}

-- | Higher-level boxed data type for parsing results.
data Result e a
  = -- | Contains return value and unconsumed input.
    Ok a
  | -- | Recoverable-by-default failure.
    Fail
  | -- | Unrecoverble-by-default error.
    Err e
  deriving (Show, Eq, Ord, Foldable, Traversable)

instance Functor (Result e) where
  fmap f (Ok x) = Ok (f x)
  fmap _f Fail = Fail
  fmap _f (Err e) = (Err e)

instance Applicative (Result e) where
  pure = Ok

  Ok f <*> res = case res of
    Ok x -> Ok $ f x
    Fail -> Fail
    Err e -> Err e
  Fail <*> _ = Fail
  Err e <*> _ = Err e

instance Monad (Result e) where
  return = pure

  Ok x >>= fr = case fr x of
    Ok x -> Ok x
    Fail -> Fail
    Err e -> Err e
  Fail >>= _ = Fail
  Err e >>= _ = Err e

instance Bifunctor Result where
  bimap f g = \case
    Ok x -> Ok $ g x
    Err e -> Err $ f e
    Fail -> Fail

instance Bifoldable Result where
  bifoldMap f g = \case
    Ok x -> g x
    Err e -> f e
    Fail -> mempty

instance Bitraversable Result where
  bitraverse f g = bisequenceA . bimap f g

instance (NFData e, NFData a) => NFData (Result e a) where
  rnf = \case
    Ok x -> rnf x
    Fail -> ()
    Err e -> rnf e

withOff# :: (Int# -> Parsec c e s a) -> Parsec c e s a
withOff# f = Parsec $ \e p@(Ix# o _) s -> runParsec# (f o) e p s
{-# INLINE withOff# #-}

withPos# :: (Int# -> Parsec c e s a) -> Parsec c e s a
withPos# f = Parsec $ \e p@(Ix# _ i) s -> runParsec# (f i) e p s
{-# INLINE withPos# #-}

getPos :: Parsec c e s Int
getPos = Parsec $ \_e p@(Ix# o _) s -> STR# s (Ok# p (I# o))

getState :: Parsec c e s s
getState = Parsec $ \_e p s -> STR# s (Ok# p s)

putState :: s -> Parsec c e s ()
putState s = Parsec $ \_e p _s -> STR# s (Ok# p ())

maybeResult :: Result e a -> Maybe a
maybeResult = \case
  Ok a -> Just a
  _ -> Nothing

-- | Throw a parsing error. By default, parser choice `(<|>)` can't backtrack
--   on parser error. Use `try` to convert an error to a recoverable failure.
err :: e -> Parsec c e s a
err e = Parsec $ \_ _ s -> STR# s (Err# e)

try :: Parsec c e s a -> Parsec c e s a
try (Parsec f) = Parsec $ \e ix s -> case f e ix s of
  STR# s r ->
    STR# s $# case r of
      Err# _ -> Fail#
      x -> x

tryWith :: Parsec c e s a -> (e -> Parsec c e s a) -> Parsec c e s a
tryWith (Parsec f) g = Parsec $ \e p s -> case f e p s of
  STR# s r -> case r of
    Err# er -> runParsec# (g er) e p s
    x -> STR# s x

parser# :: (Env# (Chunk.BaseArray# c) -> Ix# -> Res# e a) -> Parsec c e s a
parser# f = Parsec $ \e p s -> STR# s $# f e p
{-# INLINE parser# #-}

runParserRest ::
  forall chunk e s a.
  Chunk chunk =>
  Parsec chunk e s a ->
  s ->
  chunk ->
  Result e (a, Chunk.ChunkSlice chunk)
runParserRest (Parsec f) s c = case Chunk.toSlice# @chunk c of
  (# s#, off#, len# #) ->
    case (f (Env# s# len#) (Ix# 0# off#) s) of
      (# _, r #) -> case r of
        Err# e -> Err e
        Fail# -> Fail
        Ok# (Ix# _ i) a -> Ok (a, Chunk.convertSlice# @chunk (# s#, i, len# #))

runParser :: Chunk c => Parsec c e s a -> s -> c -> Result e a
runParser p s c = fst <$> runParserRest p s c
