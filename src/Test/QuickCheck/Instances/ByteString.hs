{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.ByteString () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Word              (Word8)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random (QCGen (..))

import qualified System.Random.SplitMix as SM

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-------------------------------------------------------------------------------
-- bytestring
-------------------------------------------------------------------------------

instance Arbitrary BS.ByteString where
    arbitrary = MkGen $ \(QCGen g0) size ->
        if size <= 0
        then BS.empty
        else
            let (i, g1) = SM.nextInt g0
                size' = i `mod` size
            in fst (BS.unfoldrN size' gen g1)
      where
        gen :: SM.SMGen -> Maybe (Word8, SM.SMGen)
        gen !g = Just (fromIntegral w64, g')
          where
            ~(w64, g') = SM.nextWord64 g

    shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance CoArbitrary BS.ByteString where
    coarbitrary = coarbitrary . BS.unpack

instance Function BS.ByteString where
    function = functionMap BS.unpack BS.pack


instance Arbitrary LBS.ByteString where
    arbitrary = MkGen $ \(QCGen g0) size ->
        if size <= 0
        then LBS.empty
        else
            let (i, g1) = SM.nextInt g0
                size' = i `mod` size
            in LBS.unfoldr gen (size', g1)
      where
        gen :: (Int, SM.SMGen) -> Maybe (Word8, (Int, SM.SMGen))
        gen (!i, !g)
            | i <= 0    = Nothing
            | otherwise = Just (fromIntegral w64, (i - 1, g'))
          where
            ~(w64, g') = SM.nextWord64 g

    shrink xs = LBS.pack <$> shrink (LBS.unpack xs)

instance CoArbitrary LBS.ByteString where
    coarbitrary = coarbitrary . LBS.unpack

instance Function LBS.ByteString where
    function = functionMap LBS.unpack LBS.pack


instance Arbitrary SBS.ShortByteString where
    arbitrary = SBS.pack <$> arbitrary
    shrink xs = SBS.pack <$> shrink (SBS.unpack xs)

instance CoArbitrary SBS.ShortByteString where
    coarbitrary = coarbitrary . SBS.unpack

instance Function SBS.ShortByteString where
    function = functionMap SBS.unpack SBS.pack
