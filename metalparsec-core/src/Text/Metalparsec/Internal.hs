{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Internal where

import Control.Applicative (Alternative (..), liftA2)
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

newtype Parsec c s e a = Parsec
  { runParsec# ::
      BaseEnv# c s ->
      Ix# ->
      State# RealWorld ->
      (# State# RealWorld, Res# e a #)
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

type Env# (c :: UnliftedType) s = (# c, Int#, MutVar# RealWorld s #)

type BaseEnv# c s = Env# (Chunk.BaseArray# c) s

-- o, i
type Ix# = (# Int#, Int# #)

type STR# e a = (# State# RealWorld, Res# e a #)

pattern STR# :: State# RealWorld -> Res# e a -> STR# e a
pattern STR# s x = (# s, x #)

pattern Env# :: c -> Int# -> MutVar# RealWorld s -> Env# c s
pattern Env# s l m = (# s, l, m #)

pattern Ix# :: Int# -> Int# -> Ix#
pattern Ix# i# i## = (# i#, i## #)

type ST# s a = State# s -> (# State# s, a #)

-- | Contains return value and a pointer to the rest of the input buffer.
pattern Ok# :: Ix# -> a -> Res# e a
pattern Ok# p a = (# (# p, a #) | | #)

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

($#) :: (Res# e a -> STR# e a) -> Res# e a -> STR# e a
f $# r = f r
{-# INLINE ($#) #-}

instance Functor (Parsec c s e) where
  fmap f (Parsec g) = Parsec $ \e p s -> case g e p s of
    STR# s r ->
      STR# s $# case r of
        Ok# p a -> let !b = f a in Ok# p b
        x -> unsafeCoerceRes# x

instance Applicative (Parsec c s e) where
  pure a = Parsec $ \_e p s -> STR# s (Ok# p a)

  Parsec ff <*> Parsec fa = Parsec $ \e p s -> case ff e p s of
    STR# s r -> case r of
      Ok# p f -> case fa e p s of
        STR# s r ->
          STR# s $# case r of
            Ok# p a -> let !b = f a in Ok# p b
            x -> unsafeCoerceRes# x
      x -> STR# s (unsafeCoerceRes# x)

  Parsec fa <* Parsec fb = Parsec $ \e p s -> case fa e p s of
    STR# s r -> case r of
      Ok# p a ->
        case fb e p s of
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

instance Monad (Parsec c s e) where
  return = pure
  {-# INLINE return #-}

  Parsec fa >>= f = Parsec $ \e p s -> case fa e p s of
    STR# s r -> case r of
      Ok# p a -> runParsec# (f a) e p s
      x -> STR# s (unsafeCoerceRes# x)

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Bifunctor (Parsec s u) where
  bimap f g (Parsec h) = Parsec $ \e p s -> case h e p s of
    STR# s r ->
      STR# s $# case r of
        Ok# p a -> Ok# p (g a)
        Fail# -> Fail#
        Err# er -> Err# (f er)

instance Semigroup a => Semigroup (Parsec c s e a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Parsec c s e a) where
  mempty = pure mempty

instance Alternative (Parsec c s e) where
  empty = Parsec $ \_ _ s -> STR# s Fail#

  -- Don't use this! @<|>@ is left associative, which is slower.
  Parsec f <|> Parsec g = Parsec $ \e p s ->
    case f e p s of
      STR# s r -> case r of
        Fail# -> g e p s
        x -> STR# s x

  -- Run a Parsec zero more times, collect the results in a list. Note: for optimal performance,
  -- try to avoid this. Often it is possible to get rid of the intermediate list by using a
  -- combinator or a custom Parsec.
  many (Parsec f) = Parsec (go [])
    where
      go xs e p s = case f e p s of
        STR# s r -> case r of
          Ok# p x -> go (x : xs) e p s
          Fail# -> STR# s (Ok# p $ reverse xs)
          Err# e -> STR# s (Err# e)

  -- Run a Parsec one more times, collect the results in a list. Note: for optimal performance,
  -- try to avoid this. Often it is possible to get rid of the intermediate list by using a
  -- combinator or a custom Parsec.
  -- some p = (:) <$> p <*> many p
  some p@(Parsec f) = p >>= \x -> Parsec (go [x])
    where
      go xs e p s = case f e p s of
        STR# s r -> case r of
          Ok# p x -> go (x : xs) e p s
          Fail# -> STR# s (Ok# p $ reverse xs)
          Err# e -> STR# s (Err# e)

instance MonadPlus (Parsec c s e) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadState s (Parsec c s e) where
  get = getState
  put = putState
  {-# INLINE get #-}
  {-# INLINE put #-}

instance MonadError e (Parsec c s e) where
  throwError = err
  catchError = tryWith
  {-# INLINE throwError #-}
  {-# INLINE catchError #-}

-- | Higher-level boxed data type for parsing results.
data Result e a
  = -- | Contains return value
    Ok a
  | -- | Recoverable-by-default failure.
    Fail
  | -- | Unrecoverble-by-default error.
    Err e
  deriving (Show, Eq, Ord, Foldable, Traversable)

instance Functor (Result e) where
  fmap f (Ok x) = Ok (f x)
  fmap _f Fail = Fail
  fmap _f (Err e) = Err e

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

withOff# :: (Int# -> Parsec c s e a) -> Parsec c s e a
withOff# f = Parsec $ \e p@(Ix# o _) s -> runParsec# (f o) e p s
{-# INLINE withOff# #-}

withPos# :: (Int# -> Parsec c s e a) -> Parsec c s e a
withPos# f = Parsec $ \e p@(Ix# _ i) s -> runParsec# (f i) e p s
{-# INLINE withPos# #-}

-- | Get the current position in the input.
getPos :: Parsec c s e Int
getPos = Parsec $ \_e p@(Ix# o _) s -> STR# s (Ok# p (I# o))

-- -- | Get the current state
getState :: Parsec c s e s
getState = Parsec $ \(Env# _ _ m) p s -> case readMutVar# m s of
  (# s, x #) -> STR# s (Ok# p x)

-- | Set the current state
putState :: s -> Parsec c s e ()
putState x = Parsec $ \(Env# _ _ m) p s -> case writeMutVar# m x s of
  s -> STR# s (Ok# p ())

-- | Turn a `Result` into a `Maybe`
maybeResult :: Result e a -> Maybe a
maybeResult = \case
  Ok a -> Just a
  _ -> Nothing

-- | Throw a parsing error. By default, parser choice `(<|>)` can't backtrack
-- on parser error. Use `try` to convert an error to a recoverable failure.
err :: e -> Parsec c s e a
err e = Parsec $ \_ _ s -> STR# s (Err# e)

-- | Convert a parsing error into failure.
try :: Parsec c s e a -> Parsec c s e a
try (Parsec f) = Parsec $ \e p s -> case f e p s of
  STR# s r ->
    STR# s $# case r of
      Err# _ -> Fail#
      x -> x

-- | Continue parsing from a parsing error.
tryWith :: Parsec c s e a -> (e -> Parsec c s e a) -> Parsec c s e a
tryWith (Parsec f) g = Parsec $ \e p s -> case f e p s of
  STR# s r -> case r of
    Err# er -> runParsec# (g er) e p s
    x -> STR# s x

-- | Create a parser that doesn't care about the state.
parser# :: (Env# (Chunk.BaseArray# c) s -> Ix# -> Res# e a) -> Parsec c s e a
parser# f = Parsec $ \e p s -> STR# s $# f e p
{-# INLINE parser# #-}

-- | Run a parser
runParser :: forall chunk s e a. Chunk chunk => Parsec chunk s e a -> s -> chunk -> Result e a
runParser (Parsec f) s c = case Chunk.toSlice# @chunk c of
  (# c#, off#, len# #) ->
    case runRW#
      ( \s# -> case newMutVar# s s# of
          (# s#, m# #) -> f (Env# c# len# m#) (Ix# 0# off#) s#
      ) of
      (# _, r #) -> case r of
        Err# e -> Err e
        Fail# -> Fail
        Ok# (Ix# _ _) a -> Ok a
