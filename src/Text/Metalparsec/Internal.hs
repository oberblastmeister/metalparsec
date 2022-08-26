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
import Text.Metalparsec.IntState (IntState#)

newtype Parsec s p u e a = Parsec
  { runParsec# ::
      Chunk.BaseArray# s ->
      Int# ->
      IntState# p ->
      Int# ->
      u ->
      Res# p u e a
  }

newtype ParsecT m s p u e a = MParsec
  { runParsecT# ::
      Chunk.BaseArray# s ->
      Int# ->
      IntState# p ->
      Int# ->
      u ->
      m (Res# p u e a)
  }
  
-- instance PrimMonad Parsec

type Res# p u e a = (# (# IntState# p, Int#, u, a #) | (# #) | (# e #) #)

-- | Contains return value and a pointer to the rest of the input buffer.
pattern Ok# :: IntState# p -> Int# -> u -> a -> Res# p u e a
pattern Ok# p i u a = (# (# p, i, u, a #) | | #)

-- | Constructor for errors which are by default non-recoverable.
pattern Err# :: e -> Res# p u e a
pattern Err# e = (# | | (# e #) #)

-- | Constructor for recoverable failure.
pattern Fail# :: Res# p u e a
pattern Fail# = (# | (# #) | #)

{-# COMPLETE Ok#, Err#, Fail# #-}

unsafeCoerceRes# :: Res# p u e a -> Res# p u e b
unsafeCoerceRes# = unsafeCoerce#

instance Functor (Parsec s p u e) where
  fmap f (Parsec g) = Parsec $ \s l p i u -> case g s l p i u of
    Ok# p i u a -> let !b = f a in Ok# p i u b
    x -> unsafeCoerceRes# x
  {-# INLINE fmap #-}

instance Applicative (Parsec s p u e) where
  pure a = Parsec $ \_ _ p i u -> Ok# p i u a
  {-# INLINE pure #-}

  Parsec ff <*> Parsec fa = Parsec $ \s l p i u -> case ff s l p i u of
    Ok# p i u f -> case fa s l p i u of
      Ok# p i u a -> let !b = f a in Ok# p i u b
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x
  {-# INLINE (<*>) #-}

  Parsec fa <* Parsec fb = Parsec $ \s l p i u -> case fa s l p i u of
    Ok# p i u a -> case fb s l p i u of
      Ok# p i u _ -> Ok# p i u a
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x
  {-# INLINE (<*) #-}

  Parsec fa *> Parsec fb = Parsec $ \s l p i u -> case fa s l p i u of
    Ok# p i u _ -> case fb s l p i u of
      Ok# p i u b -> Ok# p i u b
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x
  {-# INLINE (*>) #-}

instance Monad (Parsec s p u e) where
  return = pure
  {-# INLINE return #-}

  Parsec fa >>= f = Parsec $ \s l p i u -> case fa s l p i u of
    Ok# p i u a -> runParsec# (f a) s l p i u
    x -> unsafeCoerce# x
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Bifunctor (Parsec s p u) where
  bimap f g (Parsec m) = Parsec $ \s l p i u -> case m s l p i u of
    Ok# p i u a -> Ok# p i u (g a)
    Fail# -> Fail#
    Err# e -> Err# (f e)
  {-# INLINE bimap #-}

instance Semigroup a => Semigroup (Parsec s p u e a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Parsec s p u e a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

instance Alternative (Parsec s p u e) where
  empty = Parsec $ \_ _ _ _ _ -> Fail#
  {-# INLINE empty #-}

  -- \| Don't use this! @<|>@ is left associative, which is slower.
  Parsec f <|> Parsec g = Parsec $ \s l p i u ->
    case f s l p i u of
      Fail# -> g s l p i u
      x -> x
  {-# INLINE (<|>) #-}

  -- \| Run a Parsec zero or more times, collect the results in a list. Note: for optimal performance,
  --   try to avoid this. Often it is possible to get rid of the intermediate list by using a
  --   combinator or a custom Parsec.
  many (Parsec f) = Parsec (go [])
    where
      go xs s l p i u = case f s l p i u of
        Ok# p i u x -> go (x : xs) s l p i u
        Fail# -> Ok# p i u $! reverse xs
        Err# e -> Err# e
  {-# INLINE many #-}

  -- \| Run a Parsec one or more times, collect the results in a list. Note: for optimal performance,
  --   try to avoid this. Often it is possible to get rid of the intermediate list by using a
  --   combinator or a custom Parsec.
  -- some p = (:) <$> p <*> many p
  some p@(Parsec f) = p >>= \x -> Parsec (go [x])
    where
      go xs s l p i u = case f s l p i u of
        Ok# p i u x -> go (x : xs) s l p i u
        Fail# -> Ok# p i u $! reverse xs
        Err# e -> Err# e
  {-# INLINE some #-}

instance MonadPlus (Parsec s p u e) where
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

withParsecOff# :: (Int# -> Parsec s p u e a) -> Parsec s p u e a
withParsecOff# f = Parsec $ \s l p i u -> runParsec# (f i) s l p i u
{-# INLINE withParsecOff# #-}

setIntState# :: IntState# p -> Parsec s p u e ()
setIntState# is# = Parsec $ \s l p i u -> Ok# is# i u ()
{-# INLINE setIntState# #-}
