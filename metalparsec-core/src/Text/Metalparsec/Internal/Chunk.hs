{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Text.Metalparsec.Internal.Chunk where

import Data.ByteString (ByteString)
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Kind (Type)
import Data.Primitive.ByteArray (ByteArray (..))
import qualified Data.Primitive.ByteArray as ByteArray
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Exts.Compat
import qualified GHC.TypeLits as TypeLits
import qualified Text.Metalparsec.Internal.ByteArrayExt as ByteArrayExt
import qualified Text.Metalparsec.Internal.UnsafePureMutableByteArray as UnsafePureMutableByteArray
import qualified Text.Metalparsec.Internal.SizedCompat as S
import Text.Metalparsec.Internal.Util (pattern UnsafeText#)

type Slice# (s :: UnliftedType) = (# s, Int#, Int# #)

type BaseSlice# s = Slice# (BaseArray# s)

data Slice s = Slice !(BaseArray# s) !Int !Int

data Bytes = Bytes
  { bytes :: !ByteArray,
    off :: !Int,
    len :: !Int
  }

type TokenChunk c = (Chunk c, TokenOffset (Token c), NotText c)

type ByteChunk c = (Chunk c, Token c ~ Word8, IsByteArray# (BaseArray# c))

type TokenTag c = Tag (Token c)

type family NotText (s :: Type) :: Constraint where
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
  unsafeIndexWord8# :: a -> Int# -> Word8#
  unsafeCompare# :: ByteArray# -> Int# -> a -> Int# -> Int# -> Int#
  unsafeFind# :: a -> Int# -> Word8# -> Int#

class (IsArray# (BaseArray# s) (Token s), GetTokenTag (Token s)) => Chunk s where
  type Token s :: Type
  type BaseArray# s :: UnliftedType
  type ChunkSlice s :: Type
  toSlice# :: s -> BaseSlice# s
  convertSlice# :: BaseSlice# s -> ChunkSlice s

instance GetTokenTag Word8 where
  type Tag Word8 = Word8
  tokenTag = id
  {-# INLINE tokenTag #-}

instance IsArray# ByteArray# Word8 where
  unsafeIndex# (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE unsafeIndex# #-}

instance IsByteArray# ByteArray# where
  unsafeIndexChar8# = indexCharArray#
  unsafeIndexWord8# = S.indexWord8Array#
  unsafeCompare# = compareByteArrays#
  unsafeFind# = ByteArrayExt.unsafeFind#
  {-# INLINE unsafeCompare# #-}
  {-# INLINE unsafeIndexChar8# #-}
  {-# INLINE unsafeFind# #-}

instance IsArray# UnsafePureMutableByteArray.UnsafePureMutableByteArray# Word8 where
  unsafeIndex# = UnsafePureMutableByteArray.unsafeIndex#
  {-# INLINE unsafeIndex# #-}

instance IsByteArray# UnsafePureMutableByteArray.UnsafePureMutableByteArray# where
  unsafeIndexChar8# = UnsafePureMutableByteArray.unsafeIndexChar8#
  unsafeIndexWord8# = UnsafePureMutableByteArray.unsafeIndexWord8#
  unsafeCompare# = UnsafePureMutableByteArray.unsafeCompare#
  unsafeFind# = UnsafePureMutableByteArray.unsafeFind#
  {-# INLINE unsafeIndexChar8# #-}
  {-# INLINE unsafeIndexWord8# #-}
  {-# INLINE unsafeCompare# #-}
  {-# INLINE unsafeFind# #-}

instance Chunk ByteArray where
  type Token ByteArray = Word8
  type BaseArray# ByteArray = ByteArray#
  type ChunkSlice ByteArray = Bytes
  toSlice# (ByteArray bs#) = (# bs#, 0#, 0# #)
  convertSlice# (# ByteArray -> bytes, I# -> off, I# -> len #) = Bytes {bytes, off, len}
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}

instance Chunk ShortByteString where
  type Token ShortByteString = Word8
  type BaseArray# ShortByteString = ByteArray#
  type ChunkSlice ShortByteString = Bytes
  toSlice# (SBS bs#) = (# bs#, 0#, 0# #)
  convertSlice# (# ByteArray -> bytes, I# -> off, I# -> len #) = Bytes {bytes, off, len}
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}

instance Chunk Text where
  type Token Text = Word8
  type BaseArray# Text = ByteArray#
  type ChunkSlice Text = Text
  toSlice# (UnsafeText# bs# off# len#) = (# bs#, off#, len# #)
  convertSlice# (# bs#, off#, len# #) = (UnsafeText# bs# off# len#)
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}

instance Chunk ByteString where
  type Token ByteString = Word8
  type BaseArray# ByteString = UnsafePureMutableByteArray.UnsafePureMutableByteArray#
  type ChunkSlice ByteString = ByteString
  toSlice# bs = UnsafePureMutableByteArray.fromByteString# bs
  convertSlice# bs = UnsafePureMutableByteArray.sliceByteString# bs
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
