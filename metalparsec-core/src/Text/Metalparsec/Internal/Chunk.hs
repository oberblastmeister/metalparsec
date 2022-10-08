{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Internal.Chunk where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Primitive.ByteArray (ByteArray (..))
import qualified Data.Primitive.ByteArray as ByteArray
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Exts.Compat
import qualified GHC.TypeLits as TypeLits
import qualified Text.Metalparsec.Internal.ByteStringExt as ByteStringExt
import Text.Metalparsec.Internal.Util (pattern UnsafeText#)

type Slice# (s :: UnliftedType) = (# s, Int#, Int# #)

type Bytes# = Slice# ByteArray#

type BaseSlice# s = Slice# (BaseArray# s)

data Slice s = Slice !(BaseArray# s) !Int !Int

data Bytes = Bytes
  { bytes :: !ByteArray,
    off :: !Int,
    len :: !Int
  }

type TokenChunk c = (Chunk c, TokenOffset (Token c), NotText c)

type ByteChunk c = (BaseArray# c ~ ByteArray#)

type ByteSlicable c = Slicable# c ByteArray#

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

class Slicable# a (s :: UnliftedType) | a -> s where
  toSlice# :: a -> Slice# s
  convertSlice# :: Slice# s -> a

class (IsArray# (BaseArray# s) (Token s), GetTokenTag (Token s), Slicable# s (BaseArray# s)) => Chunk s where
  type Token s :: Type
  type BaseArray# s :: UnliftedType
  type ChunkSlice s :: Type

instance GetTokenTag Word8 where
  type Tag Word8 = Word8
  tokenTag = id
  {-# INLINE tokenTag #-}

instance IsArray# ByteArray# Word8 where
  unsafeIndex# (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE unsafeIndex# #-}

instance Slicable# Text ByteArray# where
  toSlice# (UnsafeText# bs# off# len#) = (# bs#, off#, len# #)
  convertSlice# (# bs#, off#, len# #) = UnsafeText# bs# off# len#
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}

instance Slicable# ByteString ByteArray# where
  toSlice# = ByteStringExt.fromByteString#
  convertSlice# = ByteStringExt.sliceByteString#
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}

instance Chunk Text where
  type Token Text = Word8
  type BaseArray# Text = ByteArray#
  type ChunkSlice Text = Text

instance Chunk ByteString where
  type Token ByteString = Word8
  type BaseArray# ByteString = ByteArray#
  type ChunkSlice ByteString = ByteString
