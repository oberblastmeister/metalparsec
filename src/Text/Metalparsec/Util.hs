module Text.Metalparsec.Util where

import GHC.Exts

unI# :: Int -> Int#
unI# (I# i#) = i#
