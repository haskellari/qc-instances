{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Containers () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Test.QuickCheck
       (Arbitrary (..), Arbitrary1 (..), CoArbitrary (..), Function (..), Gen,
       arbitrary1, chooseInt, functionMap, liftShrink2, shrink1, shuffle,
       sized)

import qualified Data.Tree as Tree

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance Arbitrary1 Tree.Tree where
    liftArbitrary arb = sized $ \n -> do
        k <- chooseInt (0, n)
        go k
      where
        go n = do -- n is the size of the trees.
            value <- arb
            pars <- arbPartition (n - 1) -- can go negative!
            forest <- traverse go pars
            return $ Tree.Node value forest

        arbPartition :: Int -> Gen [Int]
        arbPartition k = case compare k 1 of
            LT -> pure []
            EQ -> pure [1]
            GT -> do
                first <- chooseInt (1, k)
                rest <- arbPartition $ k - first
                shuffle (first : rest)

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

instance Function a => Function (Tree.Tree a) where
    function = functionMap (\(Tree.Node x xs) -> (x,xs)) (uncurry Tree.Node)
