module Data.Stream.Monadic.Unstreams where

import Data.Stream.Monadic (Stream)
import qualified Data.Stream.Monadic as S
import qualified Data.Vector.Fusion.Bundle.Monadic
import qualified Data.Vector.Fusion.Bundle.Size as Data.Vector.Fusion.Bundle.Monadic.Size
import qualified Data.Vector.Generic as V
import Data.Vector.Generic.Mutable (PrimMonad (..))
import qualified Data.Vector.Generic.Mutable as VM

unstreamMVector :: (PrimMonad m, VM.MVector v a) => Stream m a -> m (v (PrimState m) a)
unstreamMVector s = VM.munstream $ Data.Vector.Fusion.Bundle.Monadic.fromStream s Data.Vector.Fusion.Bundle.Monadic.Size.Unknown
{-# INLINE unstreamMVector #-}

-- unstreamVector :: (PrimMonad m, V.Vector v a) => Stream m a -> m (v a)
-- unstreamVector s = VM.vmunstream s
