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
import Text.Metalparsec.Util (pattern UnsafeText#)

newtype Pos# p = Pos# (# Int#, Int#, Int# #)

type role Pos# nominal

addPos1# :: Int# -> Pos# p -> Pos# p
addPos1# i# (Pos# (# off#, line#, col# #)) = Pos# (# off# +# i#, line#, col# #)
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

type Chunk c p = (BasicChunk c, TokenPos p (Token c))

type ByteChunk c p = (Chunk c p, Token c ~ Word8, BaseArray# c ~ ByteArray#, NextCharPos p)

type TokenTag c = Tag (Token c)

type NotText :: Type -> Constraint
type family NotText s where
  NotText Text = TypeLits.TypeError (TypeLits.Text "You cannot take individual bytes from Text")
  NotText _ = ()

class TokenPos p t | p -> t where
  offsetPos# :: Pos# p -> Pos# p
  nextTokenPos# :: t -> Pos# p -> Pos# p
  liftPos# :: Pos# p -> Int# -> p
  defPos# :: (# #) -> Pos# p

class (TokenPos p Word8) => NextCharPos p where
  nextCharPos :: Int# -> Pos# p -> Pos# p

class Eq (Tag t) => GetTokenTag t where
  type Tag t = (r :: Type) | r -> t
  type Tag t = t
  tokenTag :: t -> Tag t
  default tokenTag :: (t ~ Tag t) => t -> Tag t
  tokenTag = id
  {-# INLINE tokenTag #-}

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
  toSlice# (UnsafeText# bs# off# len#) = Slice# (# bs#, off#, len# #)
  convertSlice# (Slice# (# bs#, off#, len# #)) = (UnsafeText# bs# off# len#)
  unsafeIndex# _ (ByteArray -> bs) (I# -> i) = ByteArray.indexByteArray bs i
  {-# INLINE toSlice# #-}
  {-# INLINE convertSlice# #-}
  {-# INLINE unsafeIndex# #-}

newtype Pos = Pos {unPos :: Int}

instance TokenPos Pos Word8 where
  offsetPos# = addPos1# 1#
  nextTokenPos# _ pos# = pos#
  liftPos# pos# _ = Pos (I# (pos1# pos#))
  defPos# _ = Pos# (# 0#, 0#, 0# #)
  {-# INLINE nextTokenPos# #-}
  {-# INLINE liftPos# #-}
  {-# INLINE defPos# #-}

instance NextCharPos Pos where
  nextCharPos _ pos = pos
  {-# INLINE nextCharPos #-}

incCharPos :: NextCharPos p => Pos# p -> Pos# p
incCharPos = nextCharPos 1#
{-# INLINE incCharPos #-}

nextTokenCharPos :: (TokenPos p t, NextCharPos p) => t -> Pos# p -> Pos# p
nextTokenCharPos t p = incCharPos (nextTokenPos# t p)
{-# INLINE nextTokenCharPos #-}

data LineCol = LineCol {off :: !Int, line :: !Int, col :: !Int}

instance TokenPos LineCol Word8 where
  offsetPos# = addPos1# 1#
  nextTokenPos# t (Pos# (# off#, line#, col# #)) =
    if '\n' == (unsafeChr (fromIntegral t))
      then Pos# (# off# +# 1#, line# +# 1#, 0# #)
      else Pos# (# off# +# 1#, line#, col# #)
  defPos# _ = Pos# (# 0#, 1#, 1# #)
  liftPos# (Pos# (# I# -> off, I# -> line, I# -> col #)) _ = LineCol {off, line, col}
  {-# INLINE nextTokenPos# #-}
  {-# INLINE defPos# #-}
  {-# INLINE liftPos# #-}

instance NextCharPos LineCol where
  nextCharPos i# (Pos# (# off#, line#, col# #)) = (Pos# (# off#, line#, col# +# i# #))
  {-# INLINE nextCharPos #-}
