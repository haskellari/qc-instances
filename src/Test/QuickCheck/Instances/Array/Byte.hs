{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Array.Byte () where

#if MIN_VERSION_base(4,17,0)

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude
import Test.QuickCheck.Instances.Primitive ()

import Test.QuickCheck

import Data.Array.Byte (ByteArray (..))
import qualified Data.Primitive as P

-- | @since 0.3.28
instance Arbitrary ByteArray where
    arbitrary = fromP <$> arbitrary
    shrink ba = fromP <$> shrink (toP ba)

-- | @since 0.3.28
instance CoArbitrary ByteArray where
    coarbitrary ba = coarbitrary (toP ba)

-- | @since 0.3.28
instance Function ByteArray where
    function = functionMap toP fromP

toP :: ByteArray -> P.ByteArray
toP (ByteArray ba) = P.ByteArray ba

fromP :: P.ByteArray -> ByteArray
fromP (P.ByteArray ba) = ByteArray ba

#endif
