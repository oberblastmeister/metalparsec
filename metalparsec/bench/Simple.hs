{-# OPTIONS_GHC -Wno-unused-imports #-}

module Simple where

import qualified Data.ByteString.Char8 as B
import Data.String (fromString)
import qualified Data.Text as T
import Gauge
import qualified Simple.Attoparsec as Attoparsec
#ifdef FLATPARSE
import qualified Simple.FPBasic as FPBasic
import qualified Simple.FPStateful as FPStateful
#endif
import qualified Simple.Megaparsec as Megaparsec
import qualified Simple.Metalparsec as Metalparsec
import qualified Simple.MetalparsecTH as MetalparsecTH

sexpInp :: String
sexpInp =
  concat $ "(" : replicate 33333 "(foo (foo (foo ((bar baza)))))" ++ [")"]

longwsInp :: String
longwsInp = concat $ replicate 55555 "thisisalongkeyword   "

numcsvInp :: String
numcsvInp = concat ("0" : [(",  " ++ show n) | n <- [1 .. 100000 :: Int]])

readIntInp :: B.ByteString
readIntInp = "12345678910"

  
main :: Benchmark
main =
  bgroup
    "simple"
    [ sexp,
      long,
      csv
    ]
  where
    csv =
      env (pure (fromString numcsvInp :: B.ByteString, fromString numcsvInp :: T.Text)) $ \(~(numcsvInp, numcsvInpText)) ->
        bgroup
          "numeral csv"
          $ [ bench "metalparsec" $ whnf Metalparsec.runNumcsv numcsvInpText,
            bench "metalparsecth" $ whnf MetalparsecTH.runNumcsv numcsvInpText,
            bench "attoparsec" $ whnf Attoparsec.runNumcsv numcsvInp,
            bench "megaparsec" $ whnf Megaparsec.runNumcsv numcsvInp
          ] ++ flatparse
      where
        flatparse :: [Benchmark]
#ifdef FLATPARSE
        flatparse = [
            bench "fpbasic" $ whnf FPBasic.runNumcsv numcsvInp,
            bench "fpstateful" $ whnf FPStateful.runNumcsv numcsvInp
                    ]
#else
        flatparse = []
#endif

    long =
      env (pure (fromString longwsInp :: B.ByteString, fromString longwsInp :: T.Text)) $ \(~(longwsInp, longwsInpText)) ->
        bgroup
          "long keyword"
          $ [ bench "metalparsec" $ whnf Metalparsec.runLongws longwsInpText,
            bench "metalparsecth" $ whnf MetalparsecTH.runLongws longwsInpText,
            bench "attoparsec" $ whnf Attoparsec.runLongws longwsInp,
            bench "megaparsec" $ whnf Megaparsec.runLongws longwsInp
          ] ++ flatparse
      where
        flatparse :: [Benchmark]
#ifdef FLATPARSE
        flatparse = [
                bench "fpbasic" $ whnf FPBasic.runLongws longwsInp,
                bench "fpstateful" $ whnf FPStateful.runLongws longwsInp,
                           ]
#else
        flatparse = []
#endif

    sexp =
      env (pure (fromString sexpInp :: B.ByteString, fromString sexpInp :: T.Text)) $ \(~(sexpInp, sexpInpText)) ->
          bgroup
            "sexp"
            $ [ bench "metalparsec" $ whnf Metalparsec.runSexp sexpInpText,
              bench "metalparsecth" $ whnf MetalparsecTH.runSexp sexpInpText,
              bench "attoparsec" $ whnf Attoparsec.runSexp sexpInp,
              bench "megaparsec" $ whnf Megaparsec.runSexp sexpInp
            ] ++ flatparse
      where
        flatparse :: [Benchmark]
#ifdef FLATPARSE
        flatparse = [
                          bench "fpbasic" $ whnf FPBasic.runSexp sexpInp,
                          bench "fpstateful" $ whnf FPStateful.runSexp sexpInp
                           ]
#else
        flatparse = []
#endif

