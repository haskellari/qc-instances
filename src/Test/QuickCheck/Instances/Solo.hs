{-# LANGUAGE CPP              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Solo () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

#if MIN_VERSION_OneTuple(0,4,0)
import Data.Tuple.Solo (Solo (MkSolo), getSolo)
#else
import Data.Tuple.Solo (Solo (Solo), getSolo)
#define MkSolo Solo
#endif

import Test.QuickCheck

instance Arbitrary1 Solo where
  liftArbitrary = fmap MkSolo
  liftShrink shr = map MkSolo . shr . getSolo

instance Arbitrary a => Arbitrary (Solo a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance CoArbitrary a => CoArbitrary (Solo a) where
  coarbitrary = coarbitrary . getSolo

instance Function a => Function (Solo a) where
  function = functionMap getSolo MkSolo
