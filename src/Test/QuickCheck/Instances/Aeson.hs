{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Aeson () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Test.QuickCheck

import Data.Aeson
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.UnorderedContainers ()

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance Arbitrary Value where
  arbitrary = sized sizedArbitraryValue

sizedArbitraryValue :: Int -> Gen Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Null, bool, number, string]
  | otherwise = resize n' $ oneof [pure Null, bool, number, string, array, object']
  where
    n' = n `div` 2
    bool = Bool <$> arbitrary
    number = Number <$> arbitrary
    string = String <$> arbitrary
    array = Array <$> arbitrary
    object' = Object <$> arbitrary
