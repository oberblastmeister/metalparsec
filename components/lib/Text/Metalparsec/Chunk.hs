{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Chunk where

import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified
import Data.Kind (Type)
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Text (Text)
import Data.Word (Word8)
import Foreign (ForeignPtr)
import GHC.Base (unsafeChr)
import GHC.Exts
import GHC.TypeLits qualified as TypeLits
import Text.Metalparsec.IntState
import Text.Metalparsec.Util (pattern UnsafeText#)

newtype Slice# s = Slice# (# BaseArray# s, Int#, Int# #)

data Bytes = Bytes
  { bytes :: !ByteArray,
    off :: !Int,
    len :: !Int
  }

data UnliftedUnit# :: UnliftedType where
  UnliftedUnit# :: UnliftedUnit#

data UnliftedForeignPtr :: Type -> UnliftedType where
  UnliftedForeignPtr :: ForeignPtr a -> UnliftedForeignPtr a

type Chunk = PositionedChunk

type ByteChunk c = (Token c ~ Word8, BaseArray# c ~ ByteArray#, CharPositionedChunk c)

type TokenTag c = Tag (Token c)

type NotText :: Type -> Constraint
type family NotText s where
  NotText Text = TypeLits.TypeError (TypeLits.Text "You cannot take individual bytes from Text")
  NotText _ = ()

class Eq (Tag t) => GetTokenTag t where
  type Tag t = (r :: Type) | r -> t
  tokenTag :: t -> Tag t

class (GetTokenTag (Token s)) => BasicChunk s where
  type Token s :: Type
  type BaseArray# s :: UnliftedType
  type ChunkSlice s :: Type
  toSlice# :: s -> Slice# s
  convertSlice# :: Slice# s -> ChunkSlice s
  unsafeIndex# :: Proxy# s -> BaseArray# s -> Int# -> Token s

class (BasicChunk c) => PositionedChunk c where
  onToken# :: Token c -> IntState# c -> IntState# c
  defIntState# :: (# #) -> IntState# c
  onOffset# :: Int# -> IntState# c -> IntState# c

class (PositionedChunk c, Token c ~ Word8) => CharPositionedChunk c where
  onChar# :: Int# -> IntState# c -> IntState# c

instance GetTokenTag Word8 where
  type Tag Word8 = Word8
  type Tag Word8 = Word8
  tokenTag = id
  {-# INLINE tokenTag #-}

instance BasicChunk ByteArray where
  type Token ByteArray = Word8
  type BaseArray# ByteArray = ByteArray#
  type ChunkSlice ByteArray = Bytes
  toSlice# (ByteArray bs#) = Slice# (# bs#, 0#, 0# #)
  convertSlice# (Slice# (# ByteArray -> bytes, I# -> off, I# -> len #)) = Bytes {bytes, off, len}
  unsafeIndex# _ (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
  {-# INLINE unsafeIndex# #-}

instance PositionedChunk ByteArray where
  onToken# = onTokenLineCol##
  defIntState# _ = IntState# (# 0#, 0#, 0# #)
  onOffset# = add1#
  {-# INLINE onToken# #-}
  {-# INLINE defIntState# #-}
  {-# INLINE onOffset# #-}

instance BasicChunk ShortByteString where
  type Token ShortByteString = Word8
  type BaseArray# ShortByteString = ByteArray#
  type ChunkSlice ShortByteString = Bytes
  toSlice# (Data.ByteString.Short.SBS bs#) = Slice# (# bs#, 0#, 0# #)
  convertSlice# (Slice# (# ByteArray -> bytes, I# -> off, I# -> len #)) = Bytes {bytes, off, len}
  unsafeIndex# _ (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
  {-# INLINE unsafeIndex# #-}

instance PositionedChunk ShortByteString where
  onToken# = onTokenLineCol##
  defIntState# = intState0#
  onOffset# = add1#
  {-# INLINE onToken# #-}
  {-# INLINE defIntState# #-}
  {-# INLINE onOffset# #-}

instance BasicChunk Text where
  type Token Text = Word8
  type BaseArray# Text = ByteArray#
  type ChunkSlice Text = Text
  toSlice# (UnsafeText# bs# off# len#) = Slice# (# bs#, off#, len# #)
  convertSlice# (Slice# (# bs#, off#, len# #)) = (UnsafeText# bs# off# len#)
  unsafeIndex# _ (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
  {-# INLINE unsafeIndex# #-}

instance PositionedChunk Text where
  onToken# = onTokenLineCol##
  defIntState# = intState0#
  onOffset# = add1#
  {-# INLINE onToken# #-}
  {-# INLINE defIntState# #-}
  {-# INLINE onOffset# #-}

instance CharPositionedChunk Text where
  onChar# = add3#
  {-# INLINE onChar# #-}

newtype OffsetChunk c = OffsetChunk c
  deriving (BasicChunk)

instance PositionedChunk (OffsetChunk ByteArray) where
  onToken# _ is# = is#
  defIntState# _ = IntState# (# 0#, 0#, 0# #)
  onOffset# = add1#
  {-# INLINE onToken# #-}
  {-# INLINE defIntState# #-}
  {-# INLINE onOffset# #-}

onAscii :: CharPositionedChunk p => Word8 -> IntState# p -> IntState# p
onAscii t p = onChar# 1# (onToken# t (onOffset# 1# p))
{-# INLINE onAscii #-}

incPosChar :: CharPositionedChunk p => Int# -> IntState# p -> IntState# p
incPosChar off# p = onChar# 1# (onOffset# off# p)
{-# INLINE incPosChar #-}

onTokenLineCol## :: Word8 -> IntState# p2 -> IntState# p3
onTokenLineCol## t (IntState# (# off#, line#, col# #)) =
  if '\n' == (unsafeChr (fromIntegral t))
    then IntState# (# off# +# 1#, line# +# 1#, 0# #)
    else IntState# (# off# +# 1#, line#, col# #)
{-# INLINE onTokenLineCol## #-}
