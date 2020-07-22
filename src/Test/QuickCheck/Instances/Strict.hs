{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Strict () where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck

import qualified Data.Strict as S

-------------------------------------------------------------------------------
-- Pair
-------------------------------------------------------------------------------

-- | @since 0.3.24
instance Arbitrary2 S.Pair where
    liftArbitrary2 arbA arbB = (S.:!:) <$> arbA <*> arbB

    liftShrink2  shrA shrB (x S.:!: y) = uncurry (S.:!:) <$> 
        liftShrink2 shrA shrB (x, y)

-- | @since 0.3.24
instance (Arbitrary a) => Arbitrary1 (S.Pair a) where
    liftArbitrary = liftArbitrary2 arbitrary
    liftShrink = liftShrink2 shrink

-- | @since 0.3.24
instance (Arbitrary a, Arbitrary b) => Arbitrary (S.Pair a b) where
    arbitrary = arbitrary1
    shrink = shrink1

-- | @since 0.3.24
instance (Function a, Function b) => Function (S.Pair a b) where
    function = functionMap S.toLazy S.toStrict

-- | @since 0.3.24
instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (S.Pair a b)

-------------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------------

-- | @since 0.3.24
instance Arbitrary1 S.Maybe where
    liftArbitrary arb = frequency
        [ (1, pure S.Nothing)
        , (9, S.Just <$> arb)
        ]

    liftShrink _shr S.Nothing  = []
    liftShrink  shr (S.Just x) = S.Nothing : map S.Just (shr x)

-- | @since 0.3.24
instance (Arbitrary a) => Arbitrary (S.Maybe a) where
    arbitrary = arbitrary1
    shrink = shrink1

-- | @since 0.3.24
instance (Function a) => Function (S.Maybe a) where
    function = functionMap S.toLazy S.toStrict

-- | @since 0.3.24
instance (CoArbitrary a) => CoArbitrary (S.Maybe a)

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------

-- | @since 0.3.24
instance Arbitrary2 S.Either where
    liftArbitrary2 arbA arbB = oneof
        [ S.Left <$> arbA
        , S.Right <$> arbB
        ]

    liftShrink2  shrA _shrB (S.Left x)  = S.Left <$> shrA x
    liftShrink2 _shrA  shrB (S.Right y) = S.Right <$> shrB y

-- | @since 0.3.24
instance (Arbitrary a) => Arbitrary1 (S.Either a) where
    liftArbitrary = liftArbitrary2 arbitrary
    liftShrink = liftShrink2 shrink

-- | @since 0.3.24
instance (Arbitrary a, Arbitrary b) => Arbitrary (S.Either a b) where
    arbitrary = arbitrary1
    shrink = shrink1

-- | @since 0.3.24
instance (Function a, Function b) => Function (S.Either a b) where
    function = functionMap S.toLazy S.toStrict

-- | @since 0.3.24
instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (S.Either a b)

-------------------------------------------------------------------------------
-- These
-------------------------------------------------------------------------------

-- | @since 0.3.24
instance Arbitrary2 S.These where
    liftArbitrary2 arbA arbB = oneof
        [ S.This <$> arbA
        , S.That <$> arbB
        , S.These <$> arbA <*> arbB
        ]

    liftShrink2  shrA _shrB (S.This x) = S.This <$> shrA x
    liftShrink2 _shrA  shrB (S.That y) = S.That <$> shrB y
    liftShrink2  shrA  shrB (S.These x y) =
        [S.This x, S.That y] ++ [S.These x' y' | (x', y') <- liftShrink2 shrA shrB (x, y)]

-- | @since 0.3.24
instance (Arbitrary a) => Arbitrary1 (S.These a) where
    liftArbitrary = liftArbitrary2 arbitrary
    liftShrink = liftShrink2 shrink

-- | @since 0.3.24
instance (Arbitrary a, Arbitrary b) => Arbitrary (S.These a b) where
    arbitrary = arbitrary1
    shrink = shrink1

-- | @since 0.3.24
instance (Function a, Function b) => Function (S.These a b) where
    function = functionMap g f
      where
        g (S.This a)    = Left a
        g (S.That b)    = Right (Left b)
        g (S.These a b) = Right (Right (a, b))

        f (Left a)               = S.This a
        f (Right (Left b))       = S.That b
        f (Right (Right (a, b))) = S.These a b

-- | @since 0.3.24
instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (S.These a b)
