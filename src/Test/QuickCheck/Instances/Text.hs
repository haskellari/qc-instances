{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Text () where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

instance Arbitrary TS.Text where
    arbitrary = TS.pack <$> arbitrary
    shrink xs = TS.pack <$> shrink (TS.unpack xs)

instance Arbitrary TL.Text where
    arbitrary = TL.pack <$> arbitrary
    shrink xs = TL.pack <$> shrink (TL.unpack xs)

instance CoArbitrary TS.Text where
    coarbitrary = coarbitrary . TS.unpack

instance CoArbitrary TL.Text where
    coarbitrary = coarbitrary . TL.unpack

instance Function TS.Text where
    function = functionMap TS.unpack TS.pack

instance Function TL.Text where
    function = functionMap TL.unpack TL.pack
