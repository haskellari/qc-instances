{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Text.Short () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Test.QuickCheck

import qualified Data.Text.Short as T

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

instance Arbitrary T.ShortText where
    arbitrary = T.pack <$> arbitrary
    shrink xs = T.pack <$> shrink (T.unpack xs)

instance CoArbitrary T.ShortText where
    coarbitrary = coarbitrary . T.unpack

instance Function T.ShortText where
    function = functionMap T.unpack T.pack
