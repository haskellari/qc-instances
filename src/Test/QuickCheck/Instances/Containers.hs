{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Containers () where

import Prelude ()
import Prelude.Compat

import Data.Traversable (for)

import Test.QuickCheck

import qualified Data.Tree as Tree

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance Arbitrary1 Tree.Tree where
    liftArbitrary arb = go
      where
        go = sized $ \n -> do -- Sized is the size of the trees.
            value <- arb
            pars <- arbPartition (n - 1) -- can go negative!
            forest <- for pars $ \i -> resize i go
            return $ Tree.Node value forest

        arbPartition :: Int -> Gen [Int]
        arbPartition k = case compare k 1 of
            LT -> pure []
            EQ -> pure [1]
            GT -> do
                first <- elements [1..k]
                rest <- arbPartition $ k - first
                return $ first : rest

    liftShrink shr = go 
      where
        go (Tree.Node val forest) = forest ++
            [ Tree.Node e fs
            | (e, fs) <- liftShrink2 shr (liftShrink go) (val, forest)
            ]

instance Arbitrary a => Arbitrary (Tree.Tree a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary a => CoArbitrary (Tree.Tree a) where
    coarbitrary (Tree.Node val forest) =
        coarbitrary val . coarbitrary forest
