{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Text () where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink xs = T.pack <$> shrink (T.unpack xs)

instance Arbitrary LT.Text where
    arbitrary = LT.pack <$> arbitrary
    shrink xs = LT.pack <$> shrink (LT.unpack xs)

instance CoArbitrary T.Text where
    coarbitrary = coarbitrary . T.unpack

instance CoArbitrary LT.Text where
    coarbitrary = coarbitrary . LT.unpack

instance Function T.Text where
    function = functionMap T.unpack T.pack

instance Function LT.Text where
    function = functionMap LT.unpack LT.pack
