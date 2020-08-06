{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.These () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Test.QuickCheck

import Data.Functor.These (These1 (..))
import Data.These         (These (..))

-------------------------------------------------------------------------------
-- These
-------------------------------------------------------------------------------

-- | @since 0.3.23
instance Arbitrary2 These where
    liftArbitrary2 arbA arbB = oneof
        [ This <$> arbA
        , That <$> arbB
        , These <$> arbA <*> arbB
        ]

    liftShrink2  shrA _shrB (This x) = This <$> shrA x
    liftShrink2 _shrA  shrB (That y) = That <$> shrB y
    liftShrink2  shrA  shrB (These x y) =
        [This x, That y] ++ [These x' y' | (x', y') <- liftShrink2 shrA shrB (x, y)]

-- | @since 0.3.23
instance (Arbitrary a) => Arbitrary1 (These a) where
    liftArbitrary = liftArbitrary2 arbitrary
    liftShrink = liftShrink2 shrink

-- | @since 0.3.23
instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
    arbitrary = arbitrary1
    shrink = shrink1

-- | @since 0.3.23
instance (Function a, Function b) => Function (These a b) where
  function = functionMap g f
    where
      g (This a)    = Left a
      g (That b)    = Right (Left b)
      g (These a b) = Right (Right (a, b))

      f (Left a)               = This a
      f (Right (Left b))       = That b
      f (Right (Right (a, b))) = These a b

-- | @since 0.3.23
instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (These a b)

-------------------------------------------------------------------------------
-- These1
-------------------------------------------------------------------------------

-- | @since 0.3.23
instance (Arbitrary1 f, Arbitrary1 g) => Arbitrary1 (These1 f g) where
    liftArbitrary arb = oneof
        [ This1 <$> liftArbitrary arb
        , That1 <$> liftArbitrary arb
        , These1 <$> liftArbitrary arb <*> liftArbitrary arb
        ]

    liftShrink shr (This1 x) = This1 <$> liftShrink shr x
    liftShrink shr (That1 y) = That1 <$> liftShrink shr y
    liftShrink shr (These1 x y) =
        [ This1 x, That1 y ] ++
        [ These1 x' y'
        | (x', y') <- liftShrink2 (liftShrink shr) (liftShrink shr) (x, y)
        ]

-- | @since 0.3.23
instance (Arbitrary1 f, Arbitrary1 g, Arbitrary a) => Arbitrary (These1 f g a) where
    arbitrary = arbitrary1
    shrink    = shrink1
