{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Metalparsec.Internal.MutRec
  ( type (:>),
    MutRec#,
    new#,
    get#,
    push#,
    put#,
  )
where

import Data.Kind (Type)
import GHC.Exts
import GHC.TypeLits
import Unsafe.Coerce qualified

class (t :: Type) :> (ts :: [Type]) where
  -- | Get the position of @e@ in @es@.
  --
  -- /Note:/ GHC is kind enough to cache these values as they're top level CAFs,
  -- so the lookup is amortized @O(1)@ without any language level tricks.
  reifyIndex :: Int
  reifyIndex =
    -- Don't show "minimal complete definition" in haddock.
    error "unimplemented"

instance
  TypeError
    ( Text "There is no state type '" :<>: ShowType e :<>: Text "' in the context"
    ) =>
  e :> '[]
  where
  reifyIndex = error "unreachable"

instance {-# OVERLAPPING #-} e :> (e : es) where
  reifyIndex = 0

instance e :> es => e :> (x : es) where
  reifyIndex = 1 + reifyIndex @e @es

type MutRec# s ts = (# SmallMutableArray# s Any, Int# #)

pattern MutRec# :: SmallMutableArray# s Any -> Int# -> MutRec# s ts
pattern MutRec# marr len = (# marr, len #)

{-# COMPLETE MutRec# #-}

{-# INLINE MutRec# #-}

type ST# s (a :: TYPE r) = State# s -> (# State# s, a #)

new# :: ST# s (MutRec# s ts)
new# s = case newSmallArray# 0# undefined s of
  (# s, marr #) -> (# s, MutRec# marr 0# #)

push# :: t -> MutRec# s (t : ts) -> ST# s (MutRec# s ts)
push# x (MutRec# marr len) s = case len ==# (sizeofSmallMutableArray# marr) of
  1# -> case newSmallArray#
    ( let len' = len *# 2#
       in case len' ==# 0# of
            1# -> 1#
            _ -> len'
    )
    undefined
    s of
    (# s, marr' #) -> case copySmallMutableArray# marr 0# marr' 0# len s of
      s -> (# writeSmallArray# marr' len (toAny x) s, MutRec# marr' (len +# 1#) #)
  _ -> (# writeSmallArray# marr len (toAny x) s, MutRec# marr (len +# 1#) #)
{-# INLINE push# #-}

get# :: forall t ts s. t :> ts => MutRec# s ts -> ST# s t
get# (MutRec# marr len) s = case readSmallArray# marr (len -# i -# 1#) s of
  (# s, a #) -> (# s, fromAny a #)
  where
    !(I# i) = reifyIndex @t @ts
{-# INLINE get# #-}

put# :: forall t ts s. t :> ts => t -> MutRec# s ts -> ST# s (# #)
put# x (MutRec# marr len) s = case writeSmallArray# marr (len -# i -# 1#) (toAny x) s of
  s -> (# s, (# #) #)
  where
    !(I# i) = reifyIndex @t @ts
{-# INLINE put# #-}

toAny :: a -> Any
toAny = Unsafe.Coerce.unsafeCoerce
{-# INLINE toAny #-}

fromAny :: Any -> a
fromAny = Unsafe.Coerce.unsafeCoerce
{-# INLINE fromAny #-}
