{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Hashable () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Hashable (Hashable, Hashed, hashed)
#if MIN_VERSION_hashable(1,4,0)
import Data.Hashable (hashedHash)
#else
import Data.Hashable (hash)
#endif

import Test.QuickCheck

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

#if MIN_VERSION_hashable(1,2,5)
instance (Hashable a, Arbitrary a) => Arbitrary (Hashed a) where
    arbitrary = hashed <$> arbitrary

instance CoArbitrary (Hashed a) where
#if MIN_VERSION_hashable(1,4,0)
    coarbitrary x = coarbitrary (hashedHash x :: Int)
#else
    coarbitrary x = coarbitrary (hash x :: Int)
#endif
#endif
