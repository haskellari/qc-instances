{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Hashable () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Hashable (Hashable, Hashed, hashed)

import Test.QuickCheck

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

#if MIN_VERSION_hashable(1,2,5)
instance (Hashable a, Arbitrary a) => Arbitrary (Hashed a) where
    arbitrary = hashed <$> arbitrary

instance CoArbitrary (Hashed a) where
    coarbitrary x = coarbitrary (hashed x)
#endif
