{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Transformer () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Control.Applicative.Backwards (Backwards (..))
import Control.Applicative.Lift      (Lift (..))
import Data.Functor.Reverse          (Reverse (..))

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

instance Arbitrary1 f => Arbitrary1 (Backwards f) where
  liftArbitrary arb = fmap Backwards (liftArbitrary arb)
  liftShrink shr (Backwards xs) = map Backwards (liftShrink shr xs)

instance (Arbitrary1 f, Arbitrary a) => Arbitrary (Backwards f a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance Arbitrary1 f => Arbitrary1 (Reverse f) where
  liftArbitrary arb = fmap Reverse (liftArbitrary arb)
  liftShrink shr (Reverse xs) = map Reverse (liftShrink shr xs)

instance (Arbitrary1 f, Arbitrary a) => Arbitrary (Reverse f a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance Arbitrary1 f => Arbitrary1 (Lift f) where
  liftArbitrary arb = oneof
    [ fmap Pure arb
    , fmap Other (liftArbitrary arb)
    ]

  liftShrink shr (Pure x)   = map Pure (shr x)
  liftShrink shr (Other xs) = map Other (liftShrink shr xs)

instance (Arbitrary1 f, Arbitrary a) => Arbitrary (Lift f a) where
  arbitrary = arbitrary1
  shrink = shrink1
