{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Scientific () where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck

import qualified Data.Scientific as Scientific

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance Arbitrary Scientific.Scientific where
    arbitrary = do
        c <- arbitrary
        e <- arbitrary
        return $ Scientific.scientific c e
    shrink s = map (uncurry Scientific.scientific) $
        shrink (Scientific.coefficient s, Scientific.base10Exponent s)

instance CoArbitrary Scientific.Scientific where
    coarbitrary s = coarbitrary (Scientific.coefficient s, Scientific.base10Exponent s)

instance Function Scientific.Scientific where
    function = functionMap
        (\s -> (Scientific.coefficient s, Scientific.base10Exponent s))
        (uncurry Scientific.scientific)
