{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.CaseInsensitive () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Test.QuickCheck

import qualified Data.CaseInsensitive as CI

-------------------------------------------------------------------------------
-- case-insensitive
-------------------------------------------------------------------------------

instance (CI.FoldCase a, Arbitrary a) => Arbitrary (CI.CI a) where
    arbitrary = CI.mk <$> arbitrary
    shrink = fmap CI.mk . shrink . CI.original

instance CoArbitrary a => CoArbitrary (CI.CI a) where
    coarbitrary = coarbitrary . CI.original

instance (CI.FoldCase a, Function a) => Function (CI.CI a) where
    function = functionMap CI.mk CI.original
