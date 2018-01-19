{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Vector () where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck

import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Storable as SVector
import qualified Data.Vector.Unboxed as UVector

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance Arbitrary1 Vector.Vector where
    liftArbitrary = fmap Vector.fromList . liftArbitrary
    liftShrink shr = fmap Vector.fromList . liftShrink shr . Vector.toList

instance Arbitrary a => Arbitrary (Vector.Vector a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary a => CoArbitrary (Vector.Vector a) where
    coarbitrary = coarbitraryVector


instance (SVector.Storable a, Arbitrary a) => Arbitrary (SVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector

instance (SVector.Storable a, CoArbitrary a) => CoArbitrary (SVector.Vector a) where
    coarbitrary = coarbitraryVector

instance (UVector.Unbox a, Arbitrary a) => Arbitrary (UVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector

instance (UVector.Unbox a, CoArbitrary a) => CoArbitrary (UVector.Vector a) where
    coarbitrary = coarbitraryVector

arbitraryVector :: (GVector.Vector v a, Arbitrary a) => Gen (v a)
arbitraryVector = GVector.fromList `fmap` arbitrary

shrinkVector :: (GVector.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap GVector.fromList . shrink . GVector.toList

coarbitraryVector :: (GVector.Vector v a, CoArbitrary a) => v a -> Gen b -> Gen b
coarbitraryVector = coarbitrary . GVector.toList
