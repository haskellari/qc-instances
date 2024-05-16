{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Solo () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Tuple.Solo (Solo (MkSolo), getSolo)

import Test.QuickCheck

instance Arbitrary1 Solo where
  liftArbitrary = fmap MkSolo
  liftShrink shr = map MkSolo . shr . getSolo

instance Arbitrary a => Arbitrary (Solo a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance CoArbitrary a => CoArbitrary (Solo a) where
  coarbitrary = coarbitrary . getSolo

instance Function a => Function (Solo a) where
  function = functionMap getSolo MkSolo
