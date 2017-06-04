{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Instances are provided for the types in the packages:

 * array

 * bytestring

 * case-insensitive

 * containers

 * old-time

 * text

 * time

 * unordered-containers

Since all of these instances are provided as orphans, I recommend that
you do not use this library within another library module, so that you
don't impose these instances on down-stream consumers of your code.

For information on writing a test-suite with Cabal see
<http://www.haskell.org/cabal/users-guide/#test-suites>
-}
module Test.QuickCheck.Instances () where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Hashable
import Data.Ratio
import Test.QuickCheck
import Test.QuickCheck.Function

import qualified Data.Array.IArray as Array
import qualified Data.Array.Unboxed as Array
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Fixed as Fixed -- required for QC < 2.5.0
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HML
import qualified Data.Tagged as Tagged (Tagged (..))
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified Data.Time.Clock.TAI as Time
import qualified Data.Tree as Tree
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SVector
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Unboxed as UVector
import qualified System.Time as OldTime

import Debug.Trace

import Test.QuickCheck.Instances.LegacyNumeric()

-- Array

instance (Array.Ix i, Arbitrary i, Arbitrary e) => Arbitrary (Array.Array i e) where
    arbitrary = arbitraryArray
    shrink    = shrinkArray

instance (Array.IArray Array.UArray e, Array.Ix i, Arbitrary i, Arbitrary e)
        => Arbitrary (Array.UArray i e) where
    arbitrary = arbitraryArray
    shrink    = shrinkArray

instance (Array.Ix i, CoArbitrary i, CoArbitrary e) => CoArbitrary (Array.Array i e) where
    coarbitrary = coarbitraryArray

instance (Array.IArray Array.UArray e, Array.Ix i, CoArbitrary i, CoArbitrary e)
        => CoArbitrary (Array.UArray i e) where
    coarbitrary = coarbitraryArray

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

coarbitraryArray :: (Array.IArray a e, Array.Ix i, CoArbitrary i, CoArbitrary e)
                    => a i e -> Gen c -> Gen c
coarbitraryArray = coarbitrary . Array.assocs

-- vector
instance Arbitrary a => Arbitrary (Vector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector

instance CoArbitrary a => CoArbitrary (Vector.Vector a) where
    coarbitrary = coarbitraryVector

instance (SVector.Storable a, Arbitrary a) => Arbitrary (SVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector

instance (SVector.Storable a, CoArbitrary a) => CoArbitrary (SVector.Vector a) where
    coarbitrary = coarbitraryVector

instance (UVector.Unbox a, Arbitrary a) => Arbitrary (UVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector

instance (UVector.Unbox a, CoArbitrary a) => CoArbitrary (UVector.Vector a) where
    coarbitrary = coarbitraryVector

arbitraryVector :: (GVector.Vector v a, Arbitrary a) => Gen (v a)
arbitraryVector = GVector.fromList `fmap` arbitrary

shrinkVector :: (GVector.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap GVector.fromList . shrink . GVector.toList

coarbitraryVector :: (GVector.Vector v a, CoArbitrary a) => v a -> Gen b -> Gen b
coarbitraryVector = coarbitrary . GVector.toList

-- scientific
instance Arbitrary Scientific.Scientific where
    arbitrary = do
        c <- arbitrary
        e <- arbitrary
        return $ Scientific.scientific c e
    shrink s = map (uncurry Scientific.scientific) $
        shrink (Scientific.coefficient s, Scientific.base10Exponent s)

instance CoArbitrary Scientific.Scientific where
    coarbitrary s = coarbitrary (Scientific.coefficient s, Scientific.base10Exponent s)

-- ByteString
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

-- Text
instance Arbitrary TS.Text where
    arbitrary = TS.pack <$> arbitrary
    shrink xs = TS.pack <$> shrink (TS.unpack xs)

instance Arbitrary TL.Text where
    arbitrary = TL.pack <$> arbitrary
    shrink xs = TL.pack <$> shrink (TL.unpack xs)

instance CoArbitrary TS.Text where
    coarbitrary = coarbitrary . TS.unpack

instance CoArbitrary TL.Text where
    coarbitrary = coarbitrary . TL.unpack

instance Function TS.Text where
    function = functionMap TS.unpack TS.pack

instance Function TL.Text where
    function = functionMap TL.unpack TL.pack

-- Containers
#if !(MIN_VERSION_QuickCheck(2,8,2))
instance Arbitrary a => Arbitrary (IntMap.IntMap a) where
    arbitrary = IntMap.fromList <$> arbitrary
    shrink m = IntMap.fromList <$> shrink (IntMap.toList m)

instance CoArbitrary a => CoArbitrary (IntMap.IntMap a) where
    coarbitrary = coarbitrary . IntMap.toList

instance Arbitrary IntSet.IntSet where
    arbitrary = IntSet.fromList <$> arbitrary
    shrink set = IntSet.fromList <$> shrink (IntSet.toList set)

instance CoArbitrary IntSet.IntSet where
    coarbitrary = coarbitrary . IntSet.toList

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary
    shrink m = Map.fromList <$> shrink (Map.toList m)

instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (Map.Map k v) where
    coarbitrary = coarbitrary . Map.toList

instance Arbitrary a => Arbitrary (Seq.Seq a) where
    arbitrary = Seq.fromList <$> arbitrary
    shrink xs = Seq.fromList <$> shrink (toList xs)

instance CoArbitrary a => CoArbitrary (Seq.Seq a) where
    coarbitrary = coarbitrary . toList

instance (Ord a, Arbitrary a) => Arbitrary (Set.Set a) where
    arbitrary = Set.fromList <$> arbitrary
    shrink set = Set.fromList <$> shrink (Set.toList set)

instance CoArbitrary a => CoArbitrary (Set.Set a) where
    coarbitrary = coarbitrary . Set.toList

instance (Ord a, Function a) => Function (Set.Set a) where
    function = functionMap Set.toList Set.fromList
#endif

instance (Hashable a, Eq a, Arbitrary a) => Arbitrary (HS.HashSet a) where
    arbitrary = HS.fromList <$> arbitrary
    shrink hashset = HS.fromList <$> shrink (HS.toList hashset)

instance CoArbitrary a => CoArbitrary (HS.HashSet a) where
    coarbitrary = coarbitrary . HS.toList

instance (Hashable k, Eq k, Arbitrary k, Arbitrary v) => Arbitrary (HML.HashMap k v) where
    arbitrary = HML.fromList <$> arbitrary
    shrink m = HML.fromList <$> shrink (HML.toList m)

instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (HML.HashMap k v) where
    coarbitrary = coarbitrary . HML.toList

#if MIN_VERSION_hashable(1,2,5)
instance (Hashable a, Arbitrary a) => Arbitrary (Hashed a) where
    arbitrary = hashed <$> arbitrary

instance CoArbitrary (Hashed a) where
    coarbitrary x = coarbitrary (hashed x)
#endif

instance Arbitrary a => Arbitrary (Tree.Tree a) where
    arbitrary = sized $ \n -> do -- Sized is the size of the trees.
        value <- arbitrary
        pars <- arbPartition (n - 1) -- can go negative!
        forest <- forM pars $ \i -> resize i arbitrary
        return $ Tree.Node value forest
      where
        arbPartition :: Int -> Gen [Int]
        arbPartition k = case compare k 1 of
            LT -> pure []
            EQ -> pure [1]
            GT -> do
                first <- elements [1..k]
                rest <- arbPartition $ k - first
                return $ first : rest

    shrink (Tree.Node val forest) =
         forest ++ [Tree.Node e fs | (e, fs) <- shrink (val, forest)]

instance CoArbitrary a => CoArbitrary (Tree.Tree a) where
    coarbitrary (Tree.Node val forest) =
        coarbitrary val >< coarbitrary forest

-- old-time
instance Arbitrary OldTime.Month where
    arbitrary = arbitraryBoundedEnum

instance CoArbitrary OldTime.Month where
    coarbitrary = coarbitraryEnum

instance Arbitrary OldTime.Day where
    arbitrary = arbitraryBoundedEnum

instance CoArbitrary OldTime.Day where
    coarbitrary = coarbitraryEnum

instance Arbitrary OldTime.ClockTime where
    arbitrary =
        OldTime.TOD <$> choose (0, fromIntegral (maxBound :: Int32))
                    <*> choose (0, 1000000000000 - 1)
    shrink (OldTime.TOD s p) =
        [ OldTime.TOD s' p  | s' <- shrink s ] ++
        [ OldTime.TOD s  p' | p' <- shrink p ]

instance CoArbitrary OldTime.ClockTime where
    coarbitrary (OldTime.TOD s p) =
        coarbitrary s >< coarbitrary p

instance Arbitrary OldTime.TimeDiff where
    -- a bit of a cheat ...
    arbitrary =
        OldTime.normalizeTimeDiff <$>
           (OldTime.diffClockTimes <$> arbitrary <*> arbitrary)
    shrink td@(OldTime.TimeDiff year month day hour minute second picosec) =
        [ td { OldTime.tdYear    = y' } | y' <- shrink year    ] ++
        [ td { OldTime.tdMonth   = m' } | m' <- shrink month   ] ++
        [ td { OldTime.tdDay     = d' } | d' <- shrink day     ] ++
        [ td { OldTime.tdHour    = h' } | h' <- shrink hour    ] ++
        [ td { OldTime.tdMin     = m' } | m' <- shrink minute  ] ++
        [ td { OldTime.tdSec     = s' } | s' <- shrink second  ] ++
        [ td { OldTime.tdPicosec = p' } | p' <- shrink picosec ]

instance CoArbitrary OldTime.TimeDiff where
    coarbitrary (OldTime.TimeDiff year month day hour minute second picosec) =
        coarbitrary year    ><
        coarbitrary month   ><
        coarbitrary day     ><
        coarbitrary hour    ><
        coarbitrary minute  ><
        coarbitrary second  ><
        coarbitrary picosec

-- UTC only
instance Arbitrary OldTime.CalendarTime where
    arbitrary = OldTime.toUTCTime <$> arbitrary

instance CoArbitrary OldTime.CalendarTime where
    coarbitrary (OldTime.CalendarTime
                        year month day hour minute second picosec
                        wDay yDay tzName tz isDST) =
        coarbitrary year    ><
        coarbitrary month   ><
        coarbitrary day     ><
        coarbitrary hour    ><
        coarbitrary minute  ><
        coarbitrary second  ><
        coarbitrary picosec ><
        coarbitrary wDay    ><
        coarbitrary yDay    ><
        coarbitrary tzName  ><
        coarbitrary tz      ><
        coarbitrary isDST

-- time
instance Arbitrary Time.Day where
    arbitrary = Time.ModifiedJulianDay <$> (2000 +) <$> arbitrary
    shrink    = (Time.ModifiedJulianDay <$>) . shrink . Time.toModifiedJulianDay

instance CoArbitrary Time.Day where
    coarbitrary = coarbitrary . Time.toModifiedJulianDay

instance Function Time.Day where
    function = functionMap Time.toModifiedJulianDay Time.ModifiedJulianDay

instance Arbitrary Time.UniversalTime where
    arbitrary = Time.ModJulianDate <$> (2000 +) <$> arbitrary
    shrink    = (Time.ModJulianDate <$>) . shrink . Time.getModJulianDate

instance CoArbitrary Time.UniversalTime where
    coarbitrary = coarbitrary . Time.getModJulianDate

instance Arbitrary Time.DiffTime where
    arbitrary = arbitrarySizedFractional
#if MIN_VERSION_time(1,3,0)
    shrink    = shrinkRealFrac
#else
    shrink    = (fromRational <$>) . shrink . toRational
#endif

instance CoArbitrary Time.DiffTime where
    coarbitrary = coarbitraryReal

instance Function Time.DiffTime where
    function = functionMap toRational fromRational

instance Arbitrary Time.UTCTime where
    arbitrary =
        Time.UTCTime
        <$> arbitrary
        <*> (fromRational . toRational <$> choose (0::Double, 86400))
    shrink ut@(Time.UTCTime day dayTime) =
        [ ut { Time.utctDay     = d' } | d' <- shrink day     ] ++
        [ ut { Time.utctDayTime = t' } | t' <- shrink dayTime ]

instance CoArbitrary Time.UTCTime where
    coarbitrary (Time.UTCTime day dayTime) =
        coarbitrary day >< coarbitrary dayTime

instance Function Time.UTCTime where
    function = functionMap (\(Time.UTCTime day dt) -> (day,dt))
                           (uncurry Time.UTCTime)

instance Arbitrary Time.NominalDiffTime where
    arbitrary = arbitrarySizedFractional
    shrink    = shrinkRealFrac

instance CoArbitrary Time.NominalDiffTime where
    coarbitrary = coarbitraryReal

instance Arbitrary Time.TimeZone where
    arbitrary =
        Time.TimeZone
         <$> choose (-12*60,14*60) -- utc offset (m)
         <*> arbitrary -- is summer time
         <*> (sequence . replicate 4 $ choose ('A','Z'))
    shrink tz@(Time.TimeZone minutes summerOnly name) =
        [ tz { Time.timeZoneMinutes    = m' } | m' <- shrink minutes    ] ++
        [ tz { Time.timeZoneSummerOnly = s' } | s' <- shrink summerOnly ] ++
        [ tz { Time.timeZoneName       = n' } | n' <- shrink name       ]

instance CoArbitrary Time.TimeZone where
    coarbitrary (Time.TimeZone minutes summerOnly name) =
        coarbitrary minutes >< coarbitrary summerOnly >< coarbitrary name

instance Arbitrary Time.TimeOfDay where
    arbitrary =
        Time.TimeOfDay
         <$> choose (0, 23) -- hour
         <*> choose (0, 59) -- minute
         <*> (fromRational . toRational <$> choose (0::Double, 60)) -- picoseconds, via double
    shrink tod@(Time.TimeOfDay hour minute second) =
        [ tod { Time.todHour = h' } | h' <- shrink hour   ] ++
        [ tod { Time.todMin  = m' } | m' <- shrink minute ] ++
        [ tod { Time.todSec  = s' } | s' <- shrink second ]

instance CoArbitrary Time.TimeOfDay where
    coarbitrary (Time.TimeOfDay hour minute second) =
        coarbitrary hour >< coarbitrary minute >< coarbitrary second

instance Arbitrary Time.LocalTime where
    arbitrary =
        Time.LocalTime
         <$> arbitrary
         <*> arbitrary
    shrink lt@(Time.LocalTime day tod) =
        [ lt { Time.localDay       = d' } | d' <- shrink day ] ++
        [ lt { Time.localTimeOfDay = t' } | t' <- shrink tod ]

instance CoArbitrary Time.LocalTime where
    coarbitrary (Time.LocalTime day tod) =
        coarbitrary day >< coarbitrary tod

instance Arbitrary Time.ZonedTime where
    arbitrary =
        Time.ZonedTime
         <$> arbitrary
         <*> arbitrary
    shrink zt@(Time.ZonedTime lt zone) =
        [ zt { Time.zonedTimeToLocalTime = l' } | l' <- shrink lt   ] ++
        [ zt { Time.zonedTimeZone        = z' } | z' <- shrink zone ]

instance CoArbitrary Time.ZonedTime where
    coarbitrary (Time.ZonedTime lt zone) =
        coarbitrary lt >< coarbitrary zone

instance Arbitrary Time.AbsoluteTime where
    arbitrary =
        Time.addAbsoluteTime
         <$> arbitrary
         <*> return Time.taiEpoch
    shrink at =
        (`Time.addAbsoluteTime` at) <$> shrink (Time.diffAbsoluteTime at Time.taiEpoch)

instance CoArbitrary Time.AbsoluteTime where
    coarbitrary = coarbitrary . flip Time.diffAbsoluteTime Time.taiEpoch

-- Introduced in QC 2.5.0
#if !(MIN_VERSION_QuickCheck(2,5,0))
instance Arbitrary Ordering where
    arbitrary = arbitraryBoundedEnum
    shrink GT = [EQ, LT]
    shrink LT = [EQ]
    shrink EQ = []

instance CoArbitrary Ordering where
    coarbitrary GT = variant (1 :: Integer)
    coarbitrary EQ = variant (0 :: Integer)
    coarbitrary LT = variant (-1 :: Integer)

instance Fixed.HasResolution a => Arbitrary (Fixed.Fixed a) where
    arbitrary = arbitrarySizedFractional
    shrink    = shrinkRealFrac

instance Fixed.HasResolution a => CoArbitrary (Fixed.Fixed a) where
    coarbitrary = coarbitraryReal

arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
arbitraryBoundedEnum =
  do let mn = minBound
         mx = maxBound `asTypeOf` mn
     n <- choose (fromEnum mn, fromEnum mx)
     return (toEnum n `asTypeOf` mn)

coarbitraryEnum :: Enum a => a -> Gen c -> Gen c
coarbitraryEnum = variant . fromEnum
#endif

#if !(MIN_VERSION_QuickCheck(2,8,0))
instance (Function a, Integral a) => Function (Ratio a) where
    function = functionMap (numerator &&& denominator) (uncurry (%))
#endif

instance (CI.FoldCase a, Arbitrary a) => Arbitrary (CI.CI a) where
    arbitrary = CI.mk <$> arbitrary
    shrink = fmap CI.mk . shrink . CI.original

instance CoArbitrary a => CoArbitrary (CI.CI a) where
    coarbitrary = coarbitrary . CI.original

instance (CI.FoldCase a, Function a) => Function (CI.CI a) where
    function = functionMap CI.mk CI.original

-- Tagged
instance Arbitrary b => Arbitrary (Tagged.Tagged a b) where
    arbitrary = Tagged.Tagged <$> arbitrary
    shrink = fmap Tagged.Tagged . shrink . Tagged.unTagged

instance CoArbitrary b => CoArbitrary (Tagged.Tagged a b) where
    coarbitrary = coarbitrary . Tagged.unTagged

instance Function b => Function (Tagged.Tagged a b) where
    function = functionMap Tagged.unTagged Tagged.Tagged
