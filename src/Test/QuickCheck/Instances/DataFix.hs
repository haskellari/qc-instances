{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.DataFix () where

import Prelude ()
import Prelude.Compat

import Data.Fix        (Fix (..), Mu (..), Nu (..), unfoldMu, unfoldNu, foldMu, foldNu)
import Test.QuickCheck (Arbitrary (..), Arbitrary1 (..), Gen, sized)

import Math.NumberTheory.Logarithms (intLog2)

-------------------------------------------------------------------------------
-- data-fix
-------------------------------------------------------------------------------

instance Arbitrary1 f => Arbitrary (Fix f) where
    arbitrary = sized arb where
        arb :: Arbitrary1 f => Int -> Gen (Fix f)
        arb n = fmap Fix $ liftArbitrary (arb (smaller n))

        smaller n | n <= 0    = 0
                  | otherwise = intLog2 n

    shrink = go where go (Fix f) = map Fix (liftShrink go f)

instance (Arbitrary1 f, Functor f) => Arbitrary (Mu f) where
    arbitrary = unfoldMu unFix <$> arbitrary
    shrink mu = unfoldMu unFix <$> shrink (foldMu Fix mu)

instance (Arbitrary1 f, Functor f) => Arbitrary (Nu f) where
    arbitrary = unfoldNu unFix <$> arbitrary
    shrink nu = unfoldNu unFix <$> shrink (foldNu Fix nu)
