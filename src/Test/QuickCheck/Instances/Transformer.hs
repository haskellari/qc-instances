{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Transformer () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor.Sum          (Sum (..))

import Test.QuickCheck

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------

-- TODO: CoArbitrary and Function, needs Coarbitrary1 and Function1

instance (Arbitrary1 m) => Arbitrary1 (MaybeT m) where
  liftArbitrary = fmap MaybeT . liftArbitrary . liftArbitrary
  liftShrink shr (MaybeT m) = map MaybeT (liftShrink (liftShrink shr) m)

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (MaybeT m a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance (Arbitrary1 f, Arbitrary1 g) => Arbitrary1 (Sum f g) where
  liftArbitrary arb = oneof [fmap InL (liftArbitrary arb), fmap InR (liftArbitrary arb)]
  liftShrink shr (InL f) = map InL (liftShrink shr f)
  liftShrink shr (InR g) = map InR (liftShrink shr g)

instance (Arbitrary1 f, Arbitrary1 g, Arbitrary a) => Arbitrary (Sum f g a) where
  arbitrary = arbitrary1
  shrink = shrink1
