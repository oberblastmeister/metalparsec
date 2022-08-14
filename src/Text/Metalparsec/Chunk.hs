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

newtype Pos# p = Pos# (# Int#, Int#, Int# #)

addPos1# :: Pos# p -> Int# -> Pos# p
addPos1# (Pos# (# off#, line#, col# #)) i# = Pos# (# off# +# i#, line#, col# #)
{-# INLINE addPos1# #-}

pos1# :: Pos# p -> Int#
pos1# (Pos# (# off#, _, _ #)) = off#
{-# INLINE pos1# #-}

newtype Slice# s = Slice# (# Unlifted s, Int#, Int# #)

data Slice s = Slice
  { chunk :: !(Unlifted s),
    off :: !Int,
    len :: !Int
  }

type Chunk c p = (BasicChunk c, TokenPos (Token c) p)

type ByteChunk c p = (Chunk c p, Token c ~ Word8, NextCharPos p)

class TokenPos t p where
  type Pos t p = r | r -> t p
  nextTokenPos :: t -> Pos# p -> Pos# p
  constructPos :: Pos# p -> Int# -> Pos t p
  defPos :: Proxy# t -> Pos# p

class (TokenPos Word8 p) => NextCharPos p where
  nextCharPos :: Proxy# t -> Pos# p -> Pos# p

class (Eq (Token s)) => BasicChunk s where
  type Token s :: Type
  type Unlifted s :: UnliftedType
  type ChunkSlice s = (r :: Type) | r -> s
  type CanTake s :: Bool
  toSlice# :: s -> Slice# s
  convertSlice# :: Slice# s -> ChunkSlice s
  unsafeIndex# :: Proxy# s -> Unlifted s -> Int# -> Token s

instance BasicChunk ByteArray where
  type Token ByteArray = Word8
  type Unlifted ByteArray = ByteArray#
  type ChunkSlice ByteArray = Slice ByteArray
  type CanTake ByteArray = True
  toSlice# (ByteArray bs#) = Slice# (# bs#, 0#, 0# #)
  convertSlice# (Slice# (# bs#, off#, len# #)) = Slice {chunk = bs#, off = I# off#, len = I# len#}
  unsafeIndex# _ (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
  {-# INLINE unsafeIndex# #-}

instance BasicChunk ShortByteString where
  type Token ShortByteString = Word8
  type Unlifted ShortByteString = ByteArray#
  type ChunkSlice ShortByteString = Slice ShortByteString
  type CanTake ShortByteString = True
  toSlice# (Data.ByteString.Short.SBS bs#) = Slice# (# bs#, 0#, 0# #)
  convertSlice# (Slice# (# bs#, off#, len# #)) = Slice {chunk = bs#, off = I# off#, len = I# len#}
  unsafeIndex# _ (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
  {-# INLINE unsafeIndex# #-}

instance BasicChunk Text where
  type Token Text = Word8
  type Unlifted Text = ByteArray#
  type ChunkSlice Text = Text
  type CanTake Text = False
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
  nextCharPos _ pos = pos
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

instance NextCharPos 'LineColPos where
  nextCharPos _ (Pos# (# off#, line#, col# #)) = (Pos# (# off#, line#, col# +# 1# #))
  {-# INLINE nextCharPos #-}
