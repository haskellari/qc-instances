{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Void where

import Test.QuickCheck

import Data.Void (Void, absurd)

-------------------------------------------------------------------------------
-- void
-------------------------------------------------------------------------------

instance CoArbitrary Void where
    coarbitrary = absurd

-- | All @'Void' -> a@ functions are 'absurd'.
instance Function Void where
    function _ = functionVoid absurd
