{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Parser where

import Control.Applicative (liftA2)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import GHC.Exts
import Text.Metalparsec.Chunk qualified as Chunk

newtype Parser s p u e a = Parser
  { runParser# ::
      Chunk.Unlifted s ->
      Int# ->
      Chunk.Pos# p ->
      Int# ->
      u ->
      Res# p u e a
  }

type Res# p u e a = (# (# Chunk.Pos# p, Int#, u, a #) | (# #) | (# e #) #)

-- | Contains return value and a pointer to the rest of the input buffer.
pattern OK# :: Chunk.Pos# p -> Int# -> u -> a -> Res# p u e a
pattern OK# p i u a = (# (# p, i, u, a #) | | #)

-- | Constructor for errors which are by default non-recoverable.
pattern Err# :: e -> Res# p u e a
pattern Err# e = (# | | (# e #) #)

-- | Constructor for recoverable failure.
pattern Fail# :: Res# p u e a
pattern Fail# = (# | (# #) | #)

{-# COMPLETE OK#, Err#, Fail# #-}

unsafeCoerceRes# :: Res# p u e a -> Res# p u e b
unsafeCoerceRes# = unsafeCoerce#

instance Functor (Parser s p u e) where
  fmap f (Parser g) = Parser $ \s l p i u -> case g s l p i u of
    OK# p i u a -> let !b = f a in OK# p i u b
    x -> unsafeCoerceRes# x
  {-# INLINE fmap #-}

instance Applicative (Parser s p u e) where
  pure a = Parser $ \_ _ p i u -> OK# p i u a
  {-# INLINE pure #-}

  Parser ff <*> Parser fa = Parser $ \s l p i u -> case ff s l p i u of
    OK# p i u f -> case fa s l p i u of
      OK# p i u a -> let !b = f a in OK# p i u b
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x
  {-# INLINE (<*>) #-}

  Parser fa <* Parser fb = Parser $ \s l p i u -> case fa s l p i u of
    OK# p i u a -> case fb s l p i u of
      OK# p i u _ -> OK# p i u a
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x
  {-# INLINE (<*) #-}

  Parser fa *> Parser fb = Parser $ \s l p i u -> case fa s l p i u of
    OK# p i u _ -> case fb s l p i u of
      OK# p i u b -> OK# p i u b
      x -> unsafeCoerceRes# x
    x -> unsafeCoerceRes# x
  {-# INLINE (*>) #-}

instance Monad (Parser s p u e) where
  return = pure
  {-# INLINE return #-}

  Parser fa >>= f = Parser $ \s l p i u -> case fa s l p i u of
    OK# p i u a -> runParser# (f a) s l p i u
    x -> unsafeCoerce# x
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Bifunctor (Parser s p u) where
  bimap f g (Parser m) = Parser $ \s l p i u -> case m s l p i u of
    OK# p i u a -> OK# p i u (g a)
    Fail# -> Fail#
    Err# e -> Err# (f e)
  {-# INLINE bimap #-}

instance Semigroup a => Semigroup (Parser s p u e a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Parser s p u e a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

-- | Higher-level boxed data type for parsing results.
data Result e a
  = -- | Contains return value and unconsumed input.
    OK a
  | -- | Recoverable-by-default failure.
    Fail
  | -- | Unrecoverble-by-default error.
    Err e
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

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

instance Bitraversable Result
