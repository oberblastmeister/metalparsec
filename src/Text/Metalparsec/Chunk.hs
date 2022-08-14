{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Chunk where

import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified
import Data.Char qualified as Char
import Data.Kind (Type)
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Text (Text)
import Data.Text.Array qualified
import Data.Text.Internal qualified
import Data.Word (Word8)
import GHC.Exts
import GHC.TypeLits qualified as TypeLits

newtype Pos# p = Pos# (# Int#, Int#, Int# #)

addPos1# :: Pos# p -> Int# -> Pos# p
addPos1# (Pos# (# off#, line#, col# #)) i# = Pos# (# off# +# i#, line#, col# #)
{-# INLINE addPos1# #-}

pos1# :: Pos# p -> Int#
pos1# (Pos# (# off#, _, _ #)) = off#
{-# INLINE pos1# #-}

newtype Slice# s = Slice# (# BaseArray# s, Int#, Int# #)

data Slice s = Slice
  { chunk :: BaseArray# s,
    off :: !Int,
    len :: !Int
  }

type Chunk c p = (BasicChunk c, TokenPos (Token c) p)

type ByteChunk c p = (Chunk c p, Token c ~ Word8, NextCharPos p)

type TokenTag c = Tag (Token c)

type NotText :: Type -> Constraint
type family NotText s where
  NotText Text = TypeLits.TypeError (TypeLits.Text "You cannot take individual bytes from Text")
  NotText _ = ()

class Unlift s where
  type Unlifted s = (r :: UnliftedType) | r -> s
  unlift :: s -> Unlifted s
  lift :: Unlifted s -> s

class TokenPos t p where
  type Pos t p = r | r -> t p
  nextTokenPos :: t -> Pos# p -> Pos# p
  constructPos :: Pos# p -> Int# -> Pos t p
  defPos :: Proxy# t -> Pos# p

class Eq (Tag t) => GetTokenTag t where
  type Tag t = (r :: Type) | r -> t
  type Tag t = t
  tokenTag :: t -> Tag t
  default tokenTag :: (t ~ Tag t) => t -> Tag t
  tokenTag = id
  {-# INLINE tokenTag #-}

class (TokenPos Word8 p) => NextCharPos p where
  nextCharPos :: Proxy# t -> Int# -> Pos# p -> Pos# p

class (GetTokenTag (Token s)) => BasicChunk s where
  type Token s :: Type
  type BaseArray# s :: UnliftedType
  type ChunkSlice s = (r :: Type) | r -> s
  toSlice# :: s -> Slice# s
  convertSlice# :: Slice# s -> ChunkSlice s
  unsafeIndex# :: Proxy# s -> BaseArray# s -> Int# -> Token s

instance GetTokenTag Word8

instance BasicChunk ByteArray where
  type Token ByteArray = Word8
  type BaseArray# ByteArray = ByteArray#
  type ChunkSlice ByteArray = Slice ByteArray
  toSlice# (ByteArray bs#) = Slice# (# bs#, 0#, 0# #)
  convertSlice# (Slice# (# bs#, off#, len# #)) = Slice {chunk = bs#, off = I# off#, len = I# len#}
  unsafeIndex# _ (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
  {-# INLINE unsafeIndex# #-}

instance BasicChunk ShortByteString where
  type Token ShortByteString = Word8
  type BaseArray# ShortByteString = ByteArray#
  type ChunkSlice ShortByteString = Slice ShortByteString
  toSlice# (Data.ByteString.Short.SBS bs#) = Slice# (# bs#, 0#, 0# #)
  convertSlice# (Slice# (# bs#, off#, len# #)) = Slice {chunk = bs#, off = I# off#, len = I# len#}
  unsafeIndex# _ (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
  {-# INLINE unsafeIndex# #-}

instance BasicChunk Text where
  type Token Text = Word8
  type BaseArray# Text = ByteArray#
  type ChunkSlice Text = Text
  toSlice# (Data.Text.Internal.Text (Data.Text.Array.ByteArray bs#) (I# off#) (I# len#)) = Slice# (# bs#, off#, len# #)
  convertSlice# (Slice# (# bs#, off#, len# #)) = (Data.Text.Internal.Text (Data.Text.Array.ByteArray bs#) (I# off#) (I# len#))
  unsafeIndex# _ (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
  {-# INLINE unsafeIndex# #-}

data PosKind
  = SimplePos
  | LineColPos
  | CustomPos

instance TokenPos Word8 'SimplePos where
  type Pos Word8 'SimplePos = Int
  nextTokenPos _ pos# = addPos1# pos# 1#
  constructPos pos# _ = I# (pos1# pos#)
  defPos _ = Pos# (# 0#, 0#, 0# #)
  {-# INLINE nextTokenPos #-}
  {-# INLINE constructPos #-}
  {-# INLINE defPos #-}

instance NextCharPos 'SimplePos where
  nextCharPos _ _ pos = pos
  {-# INLINE nextCharPos #-}

data LineColSpan = LineColSpan {span :: Pos# 'LineColPos}

instance TokenPos Word8 'LineColPos where
  type Pos Word8 'LineColPos = LineColSpan
  nextTokenPos t (Pos# (# off#, line#, col# #)) =
    if fromIntegral (Char.ord '\n') == t
      then Pos# (# off# +# 1#, line# +# 1#, 0# #)
      else Pos# (# off# +# 1#, line#, col# #)
  defPos _ = Pos# (# 0#, 1#, 1# #)
  constructPos pos _ = LineColSpan {span = pos}
  {-# INLINE nextTokenPos #-}
  {-# INLINE defPos #-}
  {-# INLINE constructPos #-}

instance NextCharPos LineColPos where
  nextCharPos _ i# (Pos# (# off#, line#, col# #)) = (Pos# (# off#, line#, col# +# i# #))
  {-# INLINE nextCharPos #-}
