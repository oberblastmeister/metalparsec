module Expr.Common (Expr (..), Op (..)) where

import Data.Word

data Expr
  = Bin Op Expr Expr
  | Num !Int
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)
