{-# LANGUAGE PolyKinds        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Tagged () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Proxy (Proxy (Proxy))

import Test.QuickCheck

import qualified Data.Tagged as Tagged (Tagged (..))

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance Arbitrary2 Tagged.Tagged where
    liftArbitrary2 _ arb = Tagged.Tagged <$> arb
    liftShrink2 _ shr = fmap Tagged.Tagged . shr . Tagged.unTagged

instance Arbitrary1 (Tagged.Tagged a) where
    liftArbitrary arb = Tagged.Tagged <$> arb
    liftShrink shr = fmap Tagged.Tagged . shr . Tagged.unTagged

instance Arbitrary b => Arbitrary (Tagged.Tagged a b) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary b => CoArbitrary (Tagged.Tagged a b) where
    coarbitrary = coarbitrary . Tagged.unTagged

instance Function b => Function (Tagged.Tagged a b) where
    function = functionMap Tagged.unTagged Tagged.Tagged


instance Arbitrary1 Proxy where
  liftArbitrary _ = pure Proxy
  liftShrink _ _ = []

instance Arbitrary (Proxy a) where
  arbitrary = pure Proxy
  shrink _  = []

instance CoArbitrary (Proxy a) where
  coarbitrary _ = id

instance Function (Proxy a) where
  function = functionMap (const ()) (const Proxy)
