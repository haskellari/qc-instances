{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Vector () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Test.QuickCheck
import Test.QuickCheck.Function ((:->))

import qualified Data.Vector           as Vector
import qualified Data.Vector.Generic   as GVector
import qualified Data.Vector.Primitive as PVector
import qualified Data.Vector.Storable  as SVector
import qualified Data.Vector.Unboxed   as UVector

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

instance Function a => Function (Vector.Vector a) where
    function = functionVector


instance (SVector.Storable a, Arbitrary a) => Arbitrary (SVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector

instance (SVector.Storable a, CoArbitrary a) => CoArbitrary (SVector.Vector a) where
    coarbitrary = coarbitraryVector

instance (SVector.Storable a, Function a) => Function (SVector.Vector a) where
    function = functionVector

instance (UVector.Unbox a, Arbitrary a) => Arbitrary (UVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector

instance (UVector.Unbox a, CoArbitrary a) => CoArbitrary (UVector.Vector a) where
    coarbitrary = coarbitraryVector

instance (UVector.Unbox a, Function a) => Function (UVector.Vector a) where
    function = functionVector

-- | @since 0.3.32
instance (PVector.Prim a, Arbitrary a) => Arbitrary (PVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector

-- | @since 0.3.32
instance (PVector.Prim a, CoArbitrary a) => CoArbitrary (PVector.Vector a) where
    coarbitrary = coarbitraryVector

-- | @since 0.3.32
instance (PVector.Prim a, Function a) => Function (PVector.Vector a) where
    function = functionVector

arbitraryVector :: (GVector.Vector v a, Arbitrary a) => Gen (v a)
arbitraryVector = GVector.fromList `fmap` arbitrary

shrinkVector :: (GVector.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap GVector.fromList . shrink . GVector.toList

coarbitraryVector :: (GVector.Vector v a, CoArbitrary a) => v a -> Gen b -> Gen b
coarbitraryVector = coarbitrary . GVector.toList

functionVector :: (GVector.Vector v a, Function a) => (v a -> c) -> v a :-> c
functionVector = functionMap GVector.toList GVector.fromList
