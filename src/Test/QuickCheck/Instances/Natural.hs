{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Natural () where

import Prelude ()

import Numeric.Natural (Natural)

import Test.QuickCheck
       (Arbitrary (..), CoArbitrary (..), arbitrarySizedNatural,
       coarbitraryIntegral, shrinkIntegral)
import Test.QuickCheck.Function (Function (..), functionIntegral)

-------------------------------------------------------------------------------
-- nats
-------------------------------------------------------------------------------

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral

instance CoArbitrary Natural where
  coarbitrary = coarbitraryIntegral

instance Function Natural where
  function = functionIntegral
