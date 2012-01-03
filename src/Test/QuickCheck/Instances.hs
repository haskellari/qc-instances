{-# LANGUAGE FlexibleContexts #-}

{-| 
  Instances for the types in the packages:
   * array
   * bytestring
   * text
   * containers
   * old-time
   * time
-}
module Test.QuickCheck.Instances () where

import Control.Applicative
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.String (IsString, fromString)
import Test.QuickCheck

import qualified Data.Array.IArray as Array
import qualified Data.Array.Unboxed as Array
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Fixed as Fixed
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified Data.Time.Clock.TAI as Time
import qualified Data.Tree as Tree
import qualified System.Time as OldTime

-- Array

instance (Array.Ix i, Integral i, Arbitrary e) => Arbitrary (Array.Array i e) where
    arbitrary = arbitraryArray

instance (Array.IArray Array.UArray e, Array.Ix i, Integral i, Arbitrary e)
        => Arbitrary (Array.UArray i e) where
    arbitrary = arbitraryArray

arbitraryArray :: (Array.IArray a e, Array.Ix i, Integral i, Arbitrary e) => Gen (a i e)
arbitraryArray = do
      b1 <- arbitrarySizedIntegral
      b2 <- arbitrarySizedIntegral
      let bounds =
              if b1 < b2 then (b1,b2) else (b2,b1)
      elms <- vector (Array.rangeSize bounds)
      return $ Array.listArray bounds elms

-- ByteString
instance Arbitrary BS.ByteString where
    arbitrary = stringArbitrary
    shrink = stringShrink BS.unpack

instance Arbitrary BL.ByteString where
    arbitrary = stringArbitrary
    shrink = stringShrink BL.unpack

-- Text
instance Arbitrary TS.Text where
    arbitrary = stringArbitrary
    shrink = stringShrink TS.unpack

instance Arbitrary TL.Text where
    arbitrary = stringArbitrary
    shrink = stringShrink TL.unpack

-- Containers

instance Arbitrary a => Arbitrary (IntMap.IntMap a) where
    arbitrary = IntMap.fromList <$> arbitrary
    shrink map = IntMap.fromList <$> shrink (IntMap.toList map)

instance Arbitrary IntSet.IntSet where
    arbitrary = IntSet.fromList <$> arbitrary
    shrink set = IntSet.fromList <$> shrink (IntSet.toList set)

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary
    shrink map = Map.fromList <$> shrink (Map.toList map)

instance Arbitrary a => Arbitrary (Seq.Seq a) where
    arbitrary = Seq.fromList <$> arbitrary
    shrink seq = Seq.fromList <$> shrink (toList seq)

instance (Ord a, Arbitrary a) => Arbitrary (Set.Set a) where
    arbitrary = Set.fromList <$> arbitrary
    shrink set = Set.fromList <$> shrink (Set.toList set)

instance Arbitrary a => Arbitrary (Tree.Tree a) where
    arbitrary = sized $ \n ->
      do val <- arbitrary
         let n' = n `div` 2
         nodes <- 
             if n' > 0
              then do
                k <- choose (0,n')
                resize n' $ sequence [ arbitrary | _ <- [1..k] ]
              else return []
         return $ Tree.Node val nodes
    shrink (Tree.Node val forest) =
        Tree.Node <$> shrink val <*> shrink forest
         
-- old-time

instance Arbitrary OldTime.Month where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary OldTime.Day where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary OldTime.ClockTime where
    arbitrary =
        OldTime.TOD <$> choose (0, fromIntegral (maxBound :: Int32))
                    <*> choose (0, 1000000000000 - 1)

instance Arbitrary OldTime.TimeDiff where
    -- a bit of a cheat ...
    arbitrary = OldTime.diffClockTimes <$> arbitrary <*> arbitrary

-- UTC only
instance Arbitrary OldTime.CalendarTime where
    arbitrary = OldTime.toUTCTime <$> arbitrary

-- time

instance Arbitrary Time.Day where
    arbitrary =
        Time.ModifiedJulianDay
            <$> choose (gregToNum 1200 1 1, gregToNum 2999 1 1)

instance Arbitrary Time.UniversalTime where
    arbitrary =
        Time.ModJulianDate
            <$> toRational `fmap` choose (gregToNum 1200 1 1 :: Double, gregToNum 2999 1 1)

instance Arbitrary Time.DiffTime where
    arbitrary = arbitrarySizedFractional

instance Arbitrary Time.UTCTime where
    arbitrary = Time.UTCTime <$> arbitrary <*> (fromRational . toRational <$> choose (0::Double, 86400))

instance Arbitrary Time.NominalDiffTime where
    arbitrary = arbitrarySizedFractional

instance Arbitrary Time.TimeZone where
    arbitrary =
        Time.TimeZone
         <$> choose (-12*60*60,12*60*60)
         <*> arbitrary
         <*> (fmap toUpper <$> resize 4 arbitrary)

instance Arbitrary Time.TimeOfDay where
    arbitrary =
        Time.TimeOfDay
         <$> choose (0, 23) -- hour
         <*> choose (0, 59) -- minute
         <*> (fromRational . toRational <$> choose (0::Double, 60)) -- picoseconds, via double

instance Arbitrary Time.LocalTime where
    arbitrary =
        Time.LocalTime
         <$> arbitrary
         <*> arbitrary

instance Arbitrary Time.ZonedTime where
    arbitrary =
        Time.ZonedTime
         <$> arbitrary
         <*> arbitrary

instance Arbitrary Time.AbsoluteTime where
    arbitrary =
        Time.addAbsoluteTime
         <$> arbitrary
         <*> return Time.taiEpoch

-- | Given a year, month, and day return a number suitable for
-- use as a Day or UniversalTime
gregToNum :: Num a => Integer -> Int -> Int -> a
gregToNum year month day =
    fromInteger . Time.toModifiedJulianDay $ Time.fromGregorian year month day

-- WARNING: from base, should be moved to QC library
instance Arbitrary Ordering where
    arbitrary = arbitraryBoundedEnum

instance Fixed.HasResolution a => Arbitrary (Fixed.Fixed a) where
    arbitrary = arbitrarySizedFractional

-- WARNING: should be moved to QC library
arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
arbitraryBoundedEnum =
  do let mn = minBound
         mx = maxBound `asTypeOf` mn
     n <- choose (fromEnum mn, fromEnum mx)
     return (toEnum n `asTypeOf` mn)

-- helper functions

stringArbitrary :: IsString a => Gen a
stringArbitrary = fromString <$> arbitrary

stringShrink :: IsString a => (a -> String) -> a -> [a]
stringShrink toString x = fromString <$> shrink (toString x)
