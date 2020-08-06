{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Array () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Control.Applicative (liftA2)
import Data.Ix             (Ix (..))

import Test.QuickCheck

import qualified Data.Array.IArray  as Array
import qualified Data.Array.Unboxed as Array

-------------------------------------------------------------------------------
-- array
-------------------------------------------------------------------------------

instance (Num i, Ix i, Arbitrary i) => Arbitrary1 (Array.Array i) where
    liftArbitrary = liftA2 makeArray arbitrary . liftArbitrary
    liftShrink = shrinkArray

instance (Num i, Ix i, Arbitrary i, Arbitrary a) => Arbitrary (Array.Array i a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance (Ix i, CoArbitrary i, CoArbitrary a) => CoArbitrary (Array.Array i a) where
    coarbitrary arr = coarbitrary (Array.bounds arr, Array.elems arr)


instance (Num i, Ix i, Array.IArray Array.UArray a, Arbitrary i, Arbitrary a) => Arbitrary (Array.UArray i a) where
    arbitrary = liftA2 makeArray arbitrary arbitrary
    shrink = shrinkArray shrink

instance (Ix i, Array.IArray Array.UArray a, CoArbitrary i, CoArbitrary a) => CoArbitrary (Array.UArray i a) where
    coarbitrary arr = coarbitrary (Array.bounds arr, Array.elems arr)


shrinkArray
    :: (Num i, Ix i, Array.IArray arr a, Arbitrary i)
    => (a -> [a]) -> arr i a -> [arr i a]
shrinkArray shr arr =
  [ makeArray lo xs | xs <- liftShrink shr (Array.elems arr) ] ++
  [ makeArray lo' (Array.elems arr) | lo' <- shrink lo ]
  where
    (lo, _) = Array.bounds arr

makeArray :: (Num i, Ix i, Array.IArray arr a) => i -> [a] -> arr i a
makeArray lo xs = Array.listArray (lo, lo + fromIntegral (length xs - 1)) xs
