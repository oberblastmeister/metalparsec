{-# OPTIONS_GHC -ddump-simpl -ddump-stg-final -ddump-cmm -ddump-asm -ddump-to-file -dsuppress-coercions #-}

module Main where

import Control.Monad.ST (ST)
import Data.Primitive.PrimArray
import Gauge.Main

unboxed :: Int -> Int
unboxed i
  | i > 0 = unboxed $ i - 1
  | otherwise = i
{-# NOINLINE unboxed #-}

heap :: Int -> ST s Int
heap i = do
  marr <- newPrimArray 1
  writePrimArray marr 0 i
  let loop = do
        i <- readPrimArray marr 0
        if i > 0
          then do
            writePrimArray marr 0 $ i - 1
            loop
          else pure i
  loop
{-# NOINLINE heap #-}

main :: IO ()
main =
  defaultMain
    [ bgroup
        "increment"
        [ bgroup
            "heap"
            [ bench "10000" $ whnf heap 10000
            ],
          bgroup
            "unboxed"
            [ bench "10000" $ whnf unboxed 10000
            ]
        ]
    ]
