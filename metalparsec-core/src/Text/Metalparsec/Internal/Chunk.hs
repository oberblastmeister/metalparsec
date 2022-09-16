{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Internal.Chunk where

import Data.ByteString (ByteString)
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Kind (Type)
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Exts.Compat
import GHC.TypeLits qualified as TypeLits
import Text.Metalparsec.Internal.ByteArrayExt qualified as ByteArrayExt
import Text.Metalparsec.Internal.PureMutableByteArray (PureMutableByteArray#)
import Text.Metalparsec.Internal.PureMutableByteArray qualified as PureMutableByteArray
import Text.Metalparsec.Internal.Util (pattern UnsafeText#)

newtype Slice# s = Slice# {getSlice# :: (# BaseArray# s, Int#, Int# #)}

data Slice s = Slice !(BaseArray# s) !Int !Int

data Bytes = Bytes
  { bytes :: !ByteArray,
    off :: !Int,
    len :: !Int
  }

type TokenChunk c = (Chunk c, TokenOffset (Token c), NotText c)

type ByteChunk c = (Chunk c, Token c ~ Word8, IsByteArray# (BaseArray# c))

type TokenTag c = Tag (Token c)

type NotText :: Type -> Constraint
type family NotText s where
  NotText Text = TypeLits.TypeError (TypeLits.Text "Text cannot be treated as a chunk of tokens. Text is utf-8 encoded, and taking individual bytes from Text would violate utf-8. Please use the functions in Text.Metalparsec.Text")
  NotText _ = ()

class Eq (Tag t) => GetTokenTag t where
  type Tag t = (r :: Type) | r -> t
  tokenTag :: t -> Tag t

class TokenOffset t where
  tokenOffset# :: t -> Int#

class IsArray# (a :: UnliftedType) x | a -> x where
  unsafeIndex# :: a -> Int# -> x

class IsArray# a Word8 => IsByteArray# (a :: UnliftedType) where
  unsafeIndexChar8# :: a -> Int# -> Char#
  unsafeCompare# :: ByteArray# -> Int# -> a -> Int# -> Int# -> Int#
  unsafeFind# :: a -> Int# -> Word8# -> Int#

class (IsArray# (BaseArray# s) (Token s), GetTokenTag (Token s)) => Chunk s where
  type Token s :: Type
  type BaseArray# s :: UnliftedType
  type ChunkSlice s :: Type
  toSlice# :: s -> Slice# s
  convertSlice# :: Slice# s -> ChunkSlice s

instance GetTokenTag Word8 where
  type Tag Word8 = Word8
  tokenTag = id
  {-# INLINE tokenTag #-}

instance IsArray# ByteArray# Word8 where
  unsafeIndex# (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE unsafeIndex# #-}

instance IsByteArray# ByteArray# where
  unsafeIndexChar8# = indexCharArray#
  unsafeCompare# = compareByteArrays#
  unsafeFind# = ByteArrayExt.unsafeFind#
  {-# INLINE unsafeCompare# #-}
  {-# INLINE unsafeIndexChar8# #-}
  {-# INLINE unsafeFind# #-}

instance IsArray# PureMutableByteArray# Word8 where
  unsafeIndex# = PureMutableByteArray.unsafeIndex#
  {-# INLINE unsafeIndex# #-}

instance IsByteArray# PureMutableByteArray# where
  unsafeIndexChar8# = PureMutableByteArray.unsafeIndexChar8#
  unsafeCompare# = PureMutableByteArray.unsafeCompare#
  unsafeFind# = PureMutableByteArray.unsafeFind#
  {-# INLINE unsafeIndexChar8# #-}
  {-# INLINE unsafeCompare# #-}
  {-# INLINE unsafeFind# #-}

instance Chunk ByteArray where
  type Token ByteArray = Word8
  type BaseArray# ByteArray = ByteArray#
  type ChunkSlice ByteArray = Bytes
  toSlice# (ByteArray bs#) = Slice# (# bs#, 0#, 0# #)
  convertSlice# (Slice# (# ByteArray -> bytes, I# -> off, I# -> len #)) = Bytes {bytes, off, len}
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}

instance Chunk ShortByteString where
  type Token ShortByteString = Word8
  type BaseArray# ShortByteString = ByteArray#
  type ChunkSlice ShortByteString = Bytes
  toSlice# (SBS bs#) = Slice# (# bs#, 0#, 0# #)
  convertSlice# (Slice# (# ByteArray -> bytes, I# -> off, I# -> len #)) = Bytes {bytes, off, len}
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}

instance Chunk Text where
  type Token Text = Word8
  type BaseArray# Text = ByteArray#
  type ChunkSlice Text = Text
  toSlice# (UnsafeText# bs# off# len#) = Slice# (# bs#, off#, len# #)
  convertSlice# (Slice# (# bs#, off#, len# #)) = (UnsafeText# bs# off# len#)
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}

instance Chunk ByteString where
  type Token ByteString = Word8
  type BaseArray# ByteString = PureMutableByteArray#
  type ChunkSlice ByteString = ByteString
  toSlice# bs = Slice# (PureMutableByteArray.fromByteString# bs)
  convertSlice# (Slice# bs) = PureMutableByteArray.sliceByteString# bs
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
