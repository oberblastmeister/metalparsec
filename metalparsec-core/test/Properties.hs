{-# LANGUAGE TemplateHaskell #-}

module Properties where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck.All (allProperties)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Metalparsec
import Util

prop_readIntShow :: Word -> Text -> Property
prop_readIntShow w t =
  w <= fromIntegral (maxBound :: Int) ==> parse ((,) <$> readInt <*> rest) (T.pack (show i) <> t') === Ok (i, t')
  where
    t' = "asdfpoiu1324jkhadfpouiqer"
    i :: Int = fromIntegral w

$(return [])

properties :: TestTree
properties = testProperties "properties" $allProperties
