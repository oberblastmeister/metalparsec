module Expr.Common (Expr (..), Op (..)) where

import Data.Word

data Expr
  = Bin Op Expr Expr
  | Num Word64

data Op
  = Add
  | Sub
  | Mul
  | Div
