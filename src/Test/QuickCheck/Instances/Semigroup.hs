{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Semigroup () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Control.Applicative (liftA2)
import Data.List.NonEmpty  (NonEmpty (..), nonEmpty)
import Data.Maybe          (mapMaybe)

import Test.QuickCheck

import qualified Data.Semigroup as Semi

-------------------------------------------------------------------------------
-- semigroups
-------------------------------------------------------------------------------

instance Arbitrary1 NonEmpty where
  liftArbitrary arb = liftA2 (:|) arb (liftArbitrary arb)
  liftShrink shr (x :| xs) = mapMaybe nonEmpty . liftShrink shr $ x : xs

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance CoArbitrary a => CoArbitrary (NonEmpty a) where
  coarbitrary (x :| xs) = coarbitrary (x, xs)

instance Function a => Function (NonEmpty a) where
  function = functionMap g h
   where
     g (x :| xs) = (x,   xs)
     h (x,   xs) =  x :| xs


instance Arbitrary1 Semi.Min where
    liftArbitrary arb = Semi.Min <$> arb
    liftShrink shr = map Semi.Min . shr . Semi.getMin

instance Arbitrary a => Arbitrary (Semi.Min a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary a => CoArbitrary (Semi.Min a) where
    coarbitrary = coarbitrary . Semi.getMin

instance Function a => Function (Semi.Min a) where
    function = functionMap Semi.getMin Semi.Min


instance Arbitrary1 Semi.Max where
    liftArbitrary arb = Semi.Max <$> arb
    liftShrink shr = map Semi.Max . shr . Semi.getMax

instance Arbitrary a => Arbitrary (Semi.Max a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary a => CoArbitrary (Semi.Max a) where
    coarbitrary = coarbitrary . Semi.getMax

instance Function a => Function (Semi.Max a) where
    function = functionMap Semi.getMax Semi.Max


instance Arbitrary1 Semi.First where
    liftArbitrary arb = Semi.First <$> arb
    liftShrink shr = map Semi.First . shr . Semi.getFirst

instance Arbitrary a => Arbitrary (Semi.First a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary a => CoArbitrary (Semi.First a) where
    coarbitrary = coarbitrary . Semi.getFirst

instance Function a => Function (Semi.First a) where
    function = functionMap Semi.getFirst Semi.First


instance Arbitrary1 Semi.Last where
    liftArbitrary arb = Semi.Last <$> arb
    liftShrink shr = map Semi.Last . shr . Semi.getLast

instance Arbitrary a => Arbitrary (Semi.Last a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary a => CoArbitrary (Semi.Last a) where
    coarbitrary = coarbitrary . Semi.getLast

instance Function a => Function (Semi.Last a) where
    function = functionMap Semi.getLast Semi.Last


instance Arbitrary1 Semi.WrappedMonoid where
    liftArbitrary arb = Semi.WrapMonoid <$> arb
    liftShrink shr = map Semi.WrapMonoid . shr . Semi.unwrapMonoid

instance Arbitrary a => Arbitrary (Semi.WrappedMonoid a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary a => CoArbitrary (Semi.WrappedMonoid a) where
    coarbitrary = coarbitrary . Semi.unwrapMonoid

instance Function a => Function (Semi.WrappedMonoid a) where
    function = functionMap Semi.unwrapMonoid Semi.WrapMonoid


#if !MIN_VERSION_base(4,16,0)
instance Arbitrary1 Semi.Option where
    liftArbitrary arb = Semi.Option <$> liftArbitrary arb
    liftShrink shr = map Semi.Option . liftShrink shr . Semi.getOption

instance Arbitrary a => Arbitrary (Semi.Option a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary a => CoArbitrary (Semi.Option a) where
    coarbitrary = coarbitrary . Semi.getOption

instance Function a => Function (Semi.Option a) where
    function = functionMap Semi.getOption Semi.Option
#endif
