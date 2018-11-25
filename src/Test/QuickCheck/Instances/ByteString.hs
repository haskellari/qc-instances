{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.ByteString () where

import Prelude ()
import Prelude.Compat

import Data.Word (Word8)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random (QCGen (..))

import qualified System.Random.TF.Gen as TF

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS

-------------------------------------------------------------------------------
-- bytestring
-------------------------------------------------------------------------------

instance Arbitrary BS.ByteString where
    arbitrary = MkGen $ \(QCGen qcGen) size ->
        let size' = size
        in fst (BS.unfoldrN size' gen qcGen)
      where
          gen :: TF.TFGen -> Maybe (Word8, TF.TFGen)
          gen g =
              let (w32, g') = TF.next g
              in Just (fromIntegral w32, g')

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
