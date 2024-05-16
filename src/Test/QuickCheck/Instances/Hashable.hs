{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Hashable () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Hashable (Hashable, Hashed, hashed)
import Data.Hashable (hashedHash)

import Test.QuickCheck

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

instance (Hashable a, Arbitrary a) => Arbitrary (Hashed a) where
    arbitrary = hashed <$> arbitrary

instance CoArbitrary (Hashed a) where
    coarbitrary x = coarbitrary (hashedHash x :: Int)
