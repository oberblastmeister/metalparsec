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

type Chunk c p = (BasicChunk c, Updater p, Token c ~ UpdaterForToken p)

type ByteChunk c p = (Chunk c p, Token c ~ Word8, BaseArray# c ~ ByteArray#, CharUpdater p)

type TokenTag c = Tag (Token c)

type NotText :: Type -> Constraint
type family NotText s where
  NotText Text = TypeLits.TypeError (TypeLits.Text "You cannot take individual bytes from Text")
  NotText _ = ()

class Updater p where
  type UpdaterForToken p :: Type
  onOffset# :: Int# -> IntState# p -> IntState# p
  onToken# :: UpdaterForToken p -> IntState# p -> IntState# p
  defIntState# :: (# #) -> IntState# p

class (Updater p, UpdaterForToken p ~ Word8) => CharUpdater p where
  onChar# :: Int# -> IntState# p -> IntState# p

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

instance GetTokenTag Word8 where
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

data OffsetUpdater

instance Updater OffsetUpdater where
  type UpdaterForToken OffsetUpdater = Word8
  onOffset# = add1#
  onToken# _ st# = st#
  defIntState# _ = IntState# (# 0#, 0#, 0# #)
  {-# INLINE onOffset# #-}
  {-# INLINE onToken# #-}
  {-# INLINE defIntState# #-}

instance CharUpdater OffsetUpdater where
  onChar# _ pos = pos
  {-# INLINE onChar# #-}

onAscii :: CharUpdater p => Word8 -> IntState# p -> IntState# p
onAscii t p = onChar# 1# (onToken# t (onOffset# 1# p))
{-# INLINE onAscii #-}

incPosChar :: CharUpdater p => Int# -> IntState# p -> IntState# p
incPosChar off# p = onChar# 1# (onOffset# off# p)
{-# INLINE incPosChar #-}

data LineColUpdater

instance Updater LineColUpdater where
  type UpdaterForToken LineColUpdater = Word8
  onOffset# = add1#
  onToken# t (IntState# (# off#, line#, col# #)) =
    if '\n' == (unsafeChr (fromIntegral t))
      then IntState# (# off# +# 1#, line# +# 1#, 0# #)
      else IntState# (# off# +# 1#, line#, col# #)
  defIntState# _ = IntState# (# 0#, 1#, 1# #)
  {-# INLINE onOffset# #-}
  {-# INLINE onToken# #-}
  {-# INLINE defIntState# #-}

instance CharUpdater LineColUpdater where
  onChar# i# (IntState# (# off#, line#, col# #)) = (IntState# (# off#, line#, col# +# i# #))
  {-# INLINE onChar# #-}
