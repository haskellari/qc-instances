{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.UnorderedContainers () where

import Prelude ()
import Prelude.Compat

import Data.Hashable (Hashable)

import Test.QuickCheck

import qualified Data.HashMap.Lazy as HML
import qualified Data.HashSet      as HS

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance (Hashable a, Eq a, Arbitrary a) => Arbitrary (HS.HashSet a) where
    arbitrary = HS.fromList <$> arbitrary
    shrink hashset = HS.fromList <$> shrink (HS.toList hashset)

instance CoArbitrary a => CoArbitrary (HS.HashSet a) where
    coarbitrary = coarbitrary . HS.toList

instance (Hashable a, Eq a, Function a) => Function (HS.HashSet a) where
    function = functionMap HS.toList HS.fromList

instance (Hashable k, Eq k, Arbitrary k) => Arbitrary1 (HML.HashMap k) where
    liftArbitrary arb =
        HML.fromList <$> liftArbitrary (liftArbitrary2 arbitrary arb)
    liftShrink shr m =
        HML.fromList <$> liftShrink (liftShrink2 shrink shr) (HML.toList m)

instance (Hashable k, Eq k, Arbitrary k, Arbitrary v) => Arbitrary (HML.HashMap k v) where
    arbitrary = arbitrary1
    shrink = shrink1

instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (HML.HashMap k v) where
    coarbitrary = coarbitrary . HML.toList

instance (Hashable k, Eq k, Function k, Function v) => Function (HML.HashMap k v) where
    function = functionMap HML.toList HML.fromList
