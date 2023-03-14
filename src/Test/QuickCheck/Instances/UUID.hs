{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.UUID () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Word (Word32)

import Test.QuickCheck as QuickCheck

import qualified Data.UUID.Types as UUID

-------------------------------------------------------------------------------
-- uuid
-------------------------------------------------------------------------------

uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID.UUID
uuidFromWords (a,b,c,d) = UUID.fromWords a b c d

-- | Uniform distribution.
instance Arbitrary UUID.UUID where
    arbitrary = uuidFromWords <$> QuickCheck.resize 10000 arbitrary
    shrink = map uuidFromWords . shrink . UUID.toWords

instance CoArbitrary UUID.UUID where
    coarbitrary = coarbitrary . UUID.toWords

instance Function UUID.UUID where
    function = functionMap UUID.toWords uuidFromWords
