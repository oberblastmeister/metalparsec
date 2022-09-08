{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Chunk where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified
import Data.Kind (Type)
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Text (Text)
import Data.Word (Word8)
import Foreign qualified
import GHC.Exts
import GHC.ForeignPtr
  ( ForeignPtr (..),
    ForeignPtrContents (PlainPtr),
    withForeignPtr,
  )
import GHC.IO (IO (..))
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import GHC.TypeLits qualified as TypeLits
import GHC.Word (Word8 (..))
import Text.Metalparsec.Util (accursedUnutterablePerformIO, pattern UnsafeText#)

newtype UnsafePureMutableArray# = UnsafePureMutableArray# (MutableByteArray# RealWorld)

type TokenChunk c = (Chunk c, TokenOffset (Token c))

newtype Slice# s = Slice# (# BaseArray# s, Int#, Int# #)

data Slice s = Slice !(BaseArray# s) !Int !Int

data Bytes = Bytes
  { bytes :: !ByteArray,
    off :: !Int,
    len :: !Int
  }

data UnliftedUnit# :: UnliftedType where
  UnliftedUnit# :: UnliftedUnit#

data UnliftedForeignPtr :: Type -> UnliftedType where
  UnliftedForeignPtr :: ForeignPtr a -> UnliftedForeignPtr a

type ByteChunk c = (Chunk c, Token c ~ Word8, BaseArray# c ~ ByteArray#)

type TokenTag c = Tag (Token c)

type NotText :: Type -> Constraint
type family NotText s where
  NotText Text = TypeLits.TypeError (TypeLits.Text "You cannot take individual bytes from Text")
  NotText _ = ()

class Eq (Tag t) => GetTokenTag t where
  type Tag t = (r :: Type) | r -> t
  tokenTag :: t -> Tag t

class TokenOffset t where
  tokenOffset# :: t -> Int#

class IsArray# (a :: UnliftedType) x | a -> x where
  unsafeIndex# :: a -> Int# -> x

class (IsArray# (BaseArray# s) (Token s), GetTokenTag (Token s)) => Chunk s where
  type Token s :: Type
  type BaseArray# s :: UnliftedType
  type ChunkSlice s :: Type
  toSlice# :: s -> Slice# s
  convertSlice# :: Slice# s -> ChunkSlice s

instance GetTokenTag Word8 where
  type Tag Word8 = Word8
  type Tag Word8 = Word8
  tokenTag = id
  {-# INLINE tokenTag #-}

instance IsArray# ByteArray# Word8 where
  unsafeIndex# (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE unsafeIndex# #-}

instance IsArray# UnsafePureMutableArray# Word8 where
  unsafeIndex# (UnsafePureMutableArray# marr) i = accursedUnutterablePerformIO $ IO $ \s -> case readWord8Array# marr i s of
    (# s, w #) -> (# s, W8# w #)
  {-# INLINE unsafeIndex# #-}

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
  toSlice# (Data.ByteString.Short.SBS bs#) = Slice# (# bs#, 0#, 0# #)
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
  type BaseArray# ByteString = UnsafePureMutableArray#
  type ChunkSlice ByteString = ByteString
  toSlice# bs@(B.Internal.BS fp@(ForeignPtr _ fpc) l) =
    let res :: Slice ByteString = unsafeDupablePerformIO $ withForeignPtr fp $ \p -> case fpc of
          PlainPtr marr -> do
            let base = Ptr (mutableByteArrayContents# marr)
                off = p `Foreign.minusPtr` base
            pure $ Slice (UnsafePureMutableArray# marr) off (off + l)
          _ -> case B.copy bs of
            B.Internal.BS fp@(ForeignPtr _ fpc) l -> withForeignPtr fp $ \p -> case fpc of
              PlainPtr marr -> do
                let base = Ptr (mutableByteArrayContents# marr)
                    off = p `Foreign.minusPtr` base
                pure $ Slice (UnsafePureMutableArray# marr) off (off + l)
              _ -> error "should be PlainPtr"
     in case res of
          Slice marr (I# off) (I# len) -> Slice# (# marr, off, len #)
  convertSlice# (Slice# (# (UnsafePureMutableArray# marr), off, len #)) = B.Internal.BS (ForeignPtr (mutableByteArrayContents# marr `plusAddr#` off) (PlainPtr marr)) (I# (len -# off))
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
