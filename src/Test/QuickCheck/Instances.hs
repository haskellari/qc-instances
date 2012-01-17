{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| 
Instances are provided for the types in the packages:

 * array - We only provide instances for arrays inexed by
   Integral types.

 * bytestring

 * text

 * containers

 * old-time

 * time

Since all of these instances are provided as orphans, I recommend that
you do not use this library within another library module, so that you
don't impose these instances on down-stream consumers of your code.

For information on writing a test-suite with Cabal see
<http://www.haskell.org/cabal/users-guide/#test-suites>
-}
module Test.QuickCheck.Instances () where

import Control.Applicative
import Data.Foldable (toList)
import Data.Int (Int32)
import Test.QuickCheck

import qualified Data.Array.IArray as Array
import qualified Data.Array.Unboxed as Array
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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

instance (Array.Ix i, Arbitrary i, Arbitrary e) => Arbitrary (Array.Array i e) where
    arbitrary = arbitraryArray
    shrink    = shrinkArray

instance (Array.IArray Array.UArray e, Array.Ix i, Arbitrary i, Arbitrary e)
        => Arbitrary (Array.UArray i e) where
    arbitrary = arbitraryArray
    shrink    = shrinkArray

arbitraryArray :: (Array.IArray a e, Array.Ix i, Arbitrary i, Arbitrary e) => Gen (a i e)
arbitraryArray = do
      b1 <- arbitrary
      b2 <- arbitrary
      let bounds =
              if b1 < b2 then (b1,b2) else (b2,b1)
      elms <- vector (Array.rangeSize bounds)
      return $ Array.listArray bounds elms

shrinkArray :: (Array.IArray a e, Array.Ix i, Arbitrary i, Arbitrary e) => a i e -> [a i e]
shrinkArray a =
    -- Shrink each elements but don't change the size of array.
    let bounds = Array.bounds a
        elmss  = shrink <$> Array.elems a
    in Array.listArray bounds <$> elmss


-- ByteString
instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

-- Text
instance Arbitrary TS.Text where
    arbitrary = TS.pack <$> arbitrary
    shrink xs = TS.pack <$> shrink (TS.unpack xs)

instance Arbitrary TL.Text where
    arbitrary = TL.pack <$> arbitrary
    shrink xs = TL.pack <$> shrink (TL.unpack xs)

-- Containers

instance Arbitrary a => Arbitrary (IntMap.IntMap a) where
    arbitrary = IntMap.fromList <$> arbitrary
    shrink m = IntMap.fromList <$> shrink (IntMap.toList m)

instance Arbitrary IntSet.IntSet where
    arbitrary = IntSet.fromList <$> arbitrary
    shrink set = IntSet.fromList <$> shrink (IntSet.toList set)

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary
    shrink m = Map.fromList <$> shrink (Map.toList m)

instance Arbitrary a => Arbitrary (Seq.Seq a) where
    arbitrary = Seq.fromList <$> arbitrary
    shrink xs = Seq.fromList <$> shrink (toList xs)

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
    shrink (OldTime.TOD s p) =
        [ OldTime.TOD s' p  | s' <- shrink s ] ++
        [ OldTime.TOD s  p' | p' <- shrink p ]

instance Arbitrary OldTime.TimeDiff where
    -- a bit of a cheat ...
    arbitrary =
        OldTime.normalizeTimeDiff <$>
           (OldTime.diffClockTimes <$> arbitrary <*> arbitrary)
    shrink td =
        [ td { OldTime.tdYear    = y' } | y' <- shrink (OldTime.tdYear    td) ] ++
        [ td { OldTime.tdMonth   = m' } | m' <- shrink (OldTime.tdMonth   td) ] ++
        [ td { OldTime.tdDay     = d' } | d' <- shrink (OldTime.tdDay     td) ] ++
        [ td { OldTime.tdHour    = h' } | h' <- shrink (OldTime.tdHour    td) ] ++
        [ td { OldTime.tdMin     = m' } | m' <- shrink (OldTime.tdMin     td) ] ++
        [ td { OldTime.tdSec     = s' } | s' <- shrink (OldTime.tdSec     td) ] ++
        [ td { OldTime.tdPicosec = p' } | p' <- shrink (OldTime.tdPicosec td) ]

-- UTC only
instance Arbitrary OldTime.CalendarTime where
    arbitrary = OldTime.toUTCTime <$> arbitrary

-- time

instance Arbitrary Time.Day where
    arbitrary = Time.ModifiedJulianDay <$> (2000 +) <$> arbitrary
    shrink    = (Time.ModifiedJulianDay <$>) . shrink . Time.toModifiedJulianDay

instance Arbitrary Time.UniversalTime where
    arbitrary = Time.ModJulianDate <$> (2000 +) <$> arbitrary
    shrink    = (Time.ModJulianDate <$>) . shrink . Time.getModJulianDate

instance Arbitrary Time.DiffTime where
    arbitrary = arbitrarySizedFractional
#if MIN_VERSION_time(1,3,0)
    shrink    = shrinkRealFrac
#else
    shrink    = (fromRational <$>) . shrink . toRational
#endif

instance Arbitrary Time.UTCTime where
    arbitrary =
        Time.UTCTime
        <$> arbitrary
        <*> (fromRational . toRational <$> choose (0::Double, 86400))
    shrink ut =
        [ ut { Time.utctDay     = d'  } | d'  <- shrink (Time.utctDay     ut) ] ++
        [ ut { Time.utctDayTime = dt' } | dt' <- shrink (Time.utctDayTime ut) ]

instance Arbitrary Time.NominalDiffTime where
    arbitrary = arbitrarySizedFractional
    shrink    = shrinkRealFrac

instance Arbitrary Time.TimeZone where
    arbitrary =
        Time.TimeZone
         <$> choose (-12*60*60,12*60*60) -- utc offset (s)
         <*> arbitrary -- is summer time
         <*> (sequence . replicate 4 $ choose ('A','Z'))
    shrink tz =
        [ tz { Time.timeZoneMinutes    = m' } | m' <- shrink (Time.timeZoneMinutes    tz) ] ++
        [ tz { Time.timeZoneSummerOnly = s' } | s' <- shrink (Time.timeZoneSummerOnly tz) ] ++
        [ tz { Time.timeZoneName       = n' } | n' <- shrink (Time.timeZoneName       tz) ]

instance Arbitrary Time.TimeOfDay where
    arbitrary =
        Time.TimeOfDay
         <$> choose (0, 23) -- hour
         <*> choose (0, 59) -- minute
         <*> (fromRational . toRational <$> choose (0::Double, 60)) -- picoseconds, via double
    shrink tod =
        [ tod { Time.todHour = h' } | h' <- shrink (Time.todHour tod) ] ++
        [ tod { Time.todMin  = m' } | m' <- shrink (Time.todMin  tod) ] ++
        [ tod { Time.todSec  = s' } | s' <- shrink (Time.todSec  tod) ]

instance Arbitrary Time.LocalTime where
    arbitrary =
        Time.LocalTime
         <$> arbitrary
         <*> arbitrary
    shrink lt =
        [ lt { Time.localDay       = d' } | d' <- shrink (Time.localDay       lt) ] ++
        [ lt { Time.localTimeOfDay = t' } | t' <- shrink (Time.localTimeOfDay lt) ]

instance Arbitrary Time.ZonedTime where
    arbitrary =
        Time.ZonedTime
         <$> arbitrary
         <*> arbitrary
    shrink zt =
        [ zt { Time.zonedTimeToLocalTime = l' } | l' <- shrink (Time.zonedTimeToLocalTime zt) ] ++
        [ zt { Time.zonedTimeZone        = z' } | z' <- shrink (Time.zonedTimeZone        zt) ]

instance Arbitrary Time.AbsoluteTime where
    arbitrary =
        Time.addAbsoluteTime
         <$> arbitrary
         <*> return Time.taiEpoch
    shrink at =
        (`Time.addAbsoluteTime` at) <$> shrink (Time.diffAbsoluteTime at Time.taiEpoch)

-- WARNING: from base, should be moved to QC library
instance Arbitrary Ordering where
    arbitrary = arbitraryBoundedEnum

instance Fixed.HasResolution a => Arbitrary (Fixed.Fixed a) where
    arbitrary = arbitrarySizedFractional
    shrink    = shrinkRealFrac

-- WARNING: should be moved to QC library
arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
arbitraryBoundedEnum =
  do let mn = minBound
         mx = maxBound `asTypeOf` mn
     n <- choose (fromEnum mn, fromEnum mx)
     return (toEnum n `asTypeOf` mn)
