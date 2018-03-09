{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.ByteString () where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS

-------------------------------------------------------------------------------
-- bytestring
-------------------------------------------------------------------------------

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance CoArbitrary BS.ByteString where
    coarbitrary = coarbitrary . BS.unpack

instance Function BS.ByteString where
    function = functionMap BS.unpack BS.pack


instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance CoArbitrary BL.ByteString where
    coarbitrary = coarbitrary . BL.unpack

instance Function BL.ByteString where
    function = functionMap BL.unpack BL.pack


instance Arbitrary SBS.ShortByteString where
    arbitrary = SBS.pack <$> arbitrary
    shrink xs = SBS.pack <$> shrink (SBS.unpack xs)

instance CoArbitrary SBS.ShortByteString where
    coarbitrary = coarbitrary . SBS.unpack

instance Function SBS.ShortByteString where
    function = functionMap SBS.unpack SBS.pack
