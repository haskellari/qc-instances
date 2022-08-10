{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Primitive () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Word (Word8)

import Test.QuickCheck

import qualified Data.Primitive as P

-------------------------------------------------------------------------------
-- ByteArray
-------------------------------------------------------------------------------

-- | @since 0.3.28
instance Arbitrary P.ByteArray where
    arbitrary = byteArrayFromList <$> arbitrary
    shrink ba = byteArrayFromList <$> shrink (byteArrayToList ba)

-- | @since 0.3.28
instance CoArbitrary P.ByteArray where
    coarbitrary ba = coarbitrary (byteArrayToList ba)

-- | @since 0.3.28
instance Function P.ByteArray where
    function = functionMap byteArrayToList byteArrayFromList

byteArrayFromList :: [Word8] -> P.ByteArray
byteArrayFromList = P.byteArrayFromList

byteArrayToList :: P.ByteArray -> [Word8]
byteArrayToList = P.foldrByteArray (:) []
