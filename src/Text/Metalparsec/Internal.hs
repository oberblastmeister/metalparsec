{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Internal where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (MonadPlus)
import Control.Monad qualified as Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import GHC.Exts
import Text.Metalparsec.Chunk qualified as Chunk

newtype Parsec s u e a = Parsec
  { runParsec# ::
      -- stream/chunk (s)
      Chunk.BaseArray# s ->
      -- length (l)
      Int# ->
      -- index (i)
      Int# ->
      -- intstate (p)
      Int# ->
      -- user state (u)
      u ->
      -- result
      Res# u e a
  }

type Res# u e a =
  (#
    (#
      -- p
      Int#,
      -- i
      Int#,
      -- u
      u,
      -- a
      a
    #) |
    (# #) |
    (# e #)
  #)

-- | Contains return value and a pointer to the rest of the input buffer.
pattern Ok# :: Int# -> Int# -> u -> a -> Res# u e a
pattern Ok# p i u a = (# (# p, i, u, a #) | | #)

-- | Constructor for errors which are by default non-recoverable.
pattern Err# :: e -> Res# u e a
pattern Err# e = (# | | (# e #) #)

-- | Constructor for recoverable failure.
pattern Fail# :: Res# u e a
pattern Fail# = (# | (# #) | #)

{-# COMPLETE Ok#, Err#, Fail# #-}

unsafeCoerceRes# :: Res# u e a -> Res# u e b
unsafeCoerceRes# = unsafeCoerce#
{-# INLINE unsafeCoerceRes# #-}

instance Functor (Parsec s u e) where
  fmap f (Parsec g) = Parsec $ \s l i p u -> case g s l i p u of
    Ok# p i u a -> let !b = f a in Ok# p i u b
    x -> unsafeCoerceRes# x
  {-# INLINE fmap #-}

instance Applicative (Parsec s u e) where
  pure a = Parsec $ \_s _l i p u -> Ok# p i u a
  {-# INLINE pure #-}

  Parsec ff <*> Parsec fa = Parsec $ \s l i p u -> case ff s l i p u of
    Ok# p i u f -> case fa s l i p u of
      Ok# p i u a -> let !b = f a in Ok# p i u b
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x
  {-# INLINE (<*>) #-}

  Parsec fa <* Parsec fb = Parsec $ \s l i p u -> case fa s l i p u of
    Ok# p i u a -> case fb s l i p u of
      Ok# p i u _ -> Ok# p i u a
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x
  {-# INLINE (<*) #-}

  Parsec fa *> Parsec fb = Parsec $ \s l i p u -> case fa s l i p u of
    Ok# p i u _ -> case fb s l i p u of
      Ok# p i u b -> Ok# p i u b
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x
  {-# INLINE (*>) #-}

instance Monad (Parsec s u e) where
  return = pure
  {-# INLINE return #-}

  Parsec fa >>= f = Parsec $ \s l i p u -> case fa s l i p u of
    Ok# p i u a -> runParsec# (f a) s l i p u
    x -> unsafeCoerce# x
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Bifunctor (Parsec s u) where
  bimap f g (Parsec m) = Parsec $ \s l i p u -> case m s l i p u of
    Ok# p i u a -> Ok# p i u (g a)
    Fail# -> Fail#
    Err# e -> Err# (f e)
  {-# INLINE bimap #-}

instance Semigroup a => Semigroup (Parsec s u e a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Parsec s u e a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

instance Alternative (Parsec s u e) where
  empty = Parsec $ \_ _ _ _ _ -> Fail#
  {-# INLINE empty #-}

  -- \| Don't use this! @<|>@ is left associative, which is slower.
  Parsec f <|> Parsec g = Parsec $ \s l i p u ->
    case f s l i p u of
      Fail# -> g s l i p u
      x -> x
  {-# INLINE (<|>) #-}

  -- \| Run a Parsec zero more times, collect the results in a list. Note: for optimal performance,
  --   try to avoid this. Often it is possible to get rid of the intermediate list by using a
  --   combinator or a custom Parsec.
  many (Parsec f) = Parsec (go [])
    where
      go xs s l i p u = case f s l i p u of
        Ok# p i u x -> go (x : xs) s l i p u
        Fail# -> Ok# p i u $! reverse xs
        Err# e -> Err# e
  {-# INLINE many #-}

  -- \| Run a Parsec one more times, collect the results in a list. Note: for optimal performance,
  --   try to avoid this. Often it is possible to get rid of the intermediate list by using a
  --   combinator or a custom Parsec.
  -- some p = (:) <$> p <*> many p
  some p@(Parsec f) = p >>= \x -> Parsec (go [x])
    where
      go xs s l i p u = case f s l i p u of
        Ok# p i u x -> go (x : xs) s l i p u
        Fail# -> Ok# p i u $! reverse xs
        Err# e -> Err# e
  {-# INLINE some #-}

instance MonadPlus (Parsec s u e) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

-- | Higher-level boxed data type for parsing results.
data Result e a
  = -- | Contains return value and unconsumed input.
    OK a
  | -- | Recoverable-by-default failure.
    Fail
  | -- | Unrecoverble-by-default error.
    Err e
  deriving (Show, Eq, Ord, Foldable, Traversable)

instance Functor (Result e) where
  fmap f (OK x) = OK (f x)
  fmap _f Fail = Fail
  fmap _f (Err e) = (Err e)
  {-# INLINE fmap #-}

instance Applicative (Result e) where
  pure = OK

  OK f <*> res = case res of
    OK x -> OK $ f x
    Fail -> Fail
    Err e -> Err e
  Fail <*> _ = Fail
  Err e <*> _ = Err e

instance Monad (Result e) where
  return = pure

  OK x >>= fr = case fr x of
    OK x -> OK x
    Fail -> Fail
    Err e -> Err e
  Fail >>= _ = Fail
  Err e >>= _ = Err e

instance Bifunctor Result where
  bimap f g = \case
    OK x -> OK $ g x
    Err e -> Err $ f e
    Fail -> Fail

instance Bifoldable Result where
  bifoldMap f g = \case
    OK x -> g x
    Err e -> f e
    Fail -> mempty

instance Bitraversable Result where
  bitraverse f g = bisequenceA . bimap f g

withParsecOff# :: (Int# -> Parsec s u e a) -> Parsec s u e a
withParsecOff# f = Parsec $ \s l i p u -> runParsec# (f i) s l i p u
{-# INLINE withParsecOff# #-}

setInt# :: Int# -> Parsec s u e ()
setInt# p = Parsec $ \_s _l i _p u -> Ok# p i u ()
{-# INLINE setInt# #-}
