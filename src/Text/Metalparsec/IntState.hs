module Text.Metalparsec.IntState where

import GHC.Exts
import GHC.OverloadedLabels

newtype IntState# p = IntState# (# Int#, Int#, Int# #)

type role IntState# nominal

instance IsLabel "_1" (IntState# p -> Int#) where
  fromLabel = get1#
  {-# INLINE fromLabel #-}

instance IsLabel "_2" (IntState# p -> Int#) where
  fromLabel = get2#
  {-# INLINE fromLabel #-}

data IntState p = IntState !Int !Int !Int

add1# :: Int# -> IntState# p -> IntState# p
add1# i# (IntState# (# _1#, _2#, _3# #)) = IntState# (# i# +# _1#, _2#, _3# #)
{-# INLINE add1# #-}

add2# :: Int# -> IntState# p -> IntState# p
add2# i# (IntState# (# _1#, _2#, _3# #)) = IntState# (# _1#, i# +# _2#, _3# #)
{-# INLINE add2# #-}

add3# :: Int# -> IntState# p -> IntState# p
add3# i# (IntState# (# _1#, _2#, _3# #)) = IntState# (# _1#, _2#, i# +# _3# #)
{-# INLINE add3# #-}

get1# :: IntState# p -> Int#
get1# (IntState# (# _1#, _, _ #)) = _1#
{-# INLINE get1# #-}

get2# :: IntState# p -> Int#
get2# (IntState# (# _, _2#, _ #)) = _2#
{-# INLINE get2# #-}

get3# :: IntState# p -> Int#
get3# (IntState# (# _, _, _3# #)) = _3#
{-# INLINE get3# #-}

map1# :: (Int# -> Int#) -> IntState# p -> IntState# p
map1# f (IntState# (# _1#, _2#, _3# #)) = IntState# (# f _1#, _2#, _3# #)
{-# INLINE map1# #-}

map2# :: (Int# -> Int#) -> IntState# p -> IntState# p
map2# f (IntState# (# _1#, _2#, _3# #)) = IntState# (# _1#, f _2#, _3# #)
{-# INLINE map2# #-}

map3# :: (Int# -> Int#) -> IntState# p -> IntState# p
map3# f (IntState# (# _1#, _2#, _3# #)) = IntState# (# _1#, _2#, f _3# #)
{-# INLINE map3# #-}

intState0# :: (# #) -> IntState# p
intState0# _ = IntState# (# 0#, 0#, 0# #)
