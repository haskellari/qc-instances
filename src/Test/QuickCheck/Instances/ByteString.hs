{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.ByteString () where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-------------------------------------------------------------------------------
-- bytestring
-------------------------------------------------------------------------------

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance CoArbitrary BS.ByteString where
    coarbitrary = coarbitrary . BS.unpack

instance CoArbitrary BL.ByteString where
    coarbitrary = coarbitrary . BL.unpack
