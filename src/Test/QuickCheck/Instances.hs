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

import Prelude ()
import Prelude.Compat

import Control.Applicative (liftA2)
import Data.Functor.Sum (Sum (..))
import Data.Hashable (Hashable, Hashed, hashed)
import Data.Int (Int32)
import Data.Ix (Ix (..))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Traversable (for)
import Data.Word (Word32)
import Numeric.Natural (Natural)

import Test.QuickCheck
import Test.QuickCheck.Function (functionIntegral)

import qualified Data.Array.IArray as Array
import qualified Data.Array.Unboxed as Array
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashSet as HS
import qualified Data.Scientific as Scientific
import qualified Data.Tagged as Tagged (Tagged (..))
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified Data.Time.Clock.TAI as Time
import qualified Data.Tree as Tree
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Storable as SVector
import qualified Data.Vector.Unboxed as UVector
import qualified System.Time as OldTime

-------------------------------------------------------------------------------
-- array
-------------------------------------------------------------------------------

instance (Num i, Ix i, Arbitrary i) => Arbitrary1 (Array.Array i) where
    liftArbitrary = liftA2 makeArray arbitrary . liftArbitrary
    liftShrink = shrinkArray

instance (Num i, Ix i, Arbitrary i, Arbitrary a) => Arbitrary (Array.Array i a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance (Ix i, CoArbitrary i, CoArbitrary a) => CoArbitrary (Array.Array i a) where
    coarbitrary arr = coarbitrary (Array.bounds arr, Array.elems arr)


instance (Num i, Ix i, Array.IArray Array.UArray a, Arbitrary i, Arbitrary a) => Arbitrary (Array.UArray i a) where
    arbitrary = liftA2 makeArray arbitrary arbitrary
    shrink = shrinkArray shrink

instance (Ix i, Array.IArray Array.UArray a, CoArbitrary i, CoArbitrary a) => CoArbitrary (Array.UArray i a) where
    coarbitrary arr = coarbitrary (Array.bounds arr, Array.elems arr)


shrinkArray
    :: (Num i, Ix i, Array.IArray arr a, Arbitrary i)
    => (a -> [a]) -> arr i a -> [arr i a]
shrinkArray shr arr =
  [ makeArray lo xs | xs <- liftShrink shr (Array.elems arr) ] ++
  [ makeArray lo' (Array.elems arr) | lo' <- shrink lo ]
  where
    (lo, _) = Array.bounds arr

makeArray :: (Num i, Ix i, Array.IArray arr a) => i -> [a] -> arr i a
makeArray lo xs = Array.listArray (lo, lo + fromIntegral (length xs - 1)) xs

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance Arbitrary1 Vector.Vector where
    liftArbitrary = fmap Vector.fromList . liftArbitrary
    liftShrink shr = fmap Vector.fromList . liftShrink shr . Vector.toList

instance Arbitrary a => Arbitrary (Vector.Vector a) where
    arbitrary = arbitrary1
    shrink = shrink1

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

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance Arbitrary Scientific.Scientific where
    arbitrary = do
        c <- arbitrary
        e <- arbitrary
        return $ Scientific.scientific c e
    shrink s = map (uncurry Scientific.scientific) $
        shrink (Scientific.coefficient s, Scientific.base10Exponent s)

instance CoArbitrary Scientific.Scientific where
    coarbitrary s = coarbitrary (Scientific.coefficient s, Scientific.base10Exponent s)

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

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance (Hashable a, Eq a, Arbitrary a) => Arbitrary (HS.HashSet a) where
    arbitrary = HS.fromList <$> arbitrary
    shrink hashset = HS.fromList <$> shrink (HS.toList hashset)

instance CoArbitrary a => CoArbitrary (HS.HashSet a) where
    coarbitrary = coarbitrary . HS.toList

instance (Hashable k, Eq k, Arbitrary k) => Arbitrary1 (HML.HashMap k) where
    liftArbitrary arb =
        HML.fromList <$> liftArbitrary (liftArbitrary2 arbitrary arb)
    liftShrink shr m =
        HML.fromList <$> liftShrink (liftShrink2 shrink shr) (HML.toList m)

instance (Hashable k, Eq k, Arbitrary k, Arbitrary v) => Arbitrary (HML.HashMap k v) where
    arbitrary = arbitrary1
    shrink = shrink1

instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (HML.HashMap k v) where
    coarbitrary = coarbitrary . HML.toList

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

#if MIN_VERSION_hashable(1,2,5)
instance (Hashable a, Arbitrary a) => Arbitrary (Hashed a) where
    arbitrary = hashed <$> arbitrary

instance CoArbitrary (Hashed a) where
    coarbitrary x = coarbitrary (hashed x)
#endif

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance Arbitrary1 Tree.Tree where
    liftArbitrary arb = go
      where
        go = sized $ \n -> do -- Sized is the size of the trees.
            value <- arb
            pars <- arbPartition (n - 1) -- can go negative!
            forest <- for pars $ \i -> resize i go
            return $ Tree.Node value forest

        arbPartition :: Int -> Gen [Int]
        arbPartition k = case compare k 1 of
            LT -> pure []
            EQ -> pure [1]
            GT -> do
                first <- elements [1..k]
                rest <- arbPartition $ k - first
                return $ first : rest

    liftShrink shr = go 
      where
        go (Tree.Node val forest) = forest ++
            [ Tree.Node e fs
            | (e, fs) <- liftShrink2 shr (liftShrink go) (val, forest)
            ]

instance Arbitrary a => Arbitrary (Tree.Tree a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance CoArbitrary a => CoArbitrary (Tree.Tree a) where
    coarbitrary (Tree.Node val forest) =
        coarbitrary val . coarbitrary forest

-------------------------------------------------------------------------------
-- old-time
-------------------------------------------------------------------------------

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
        coarbitrary s . coarbitrary p

instance Arbitrary OldTime.TimeDiff where
    -- a bit of a cheat ...
    arbitrary =
        OldTime.normalizeTimeDiff <$>
           (OldTime.diffClockTimes <$> arbitrary <*> arbitrary)
    shrink td@(OldTime.TimeDiff year month day hour minute sec picosec) =
        [ td { OldTime.tdYear    = y' } | y' <- shrink year    ] ++
        [ td { OldTime.tdMonth   = m' } | m' <- shrink month   ] ++
        [ td { OldTime.tdDay     = d' } | d' <- shrink day     ] ++
        [ td { OldTime.tdHour    = h' } | h' <- shrink hour    ] ++
        [ td { OldTime.tdMin     = m' } | m' <- shrink minute  ] ++
        [ td { OldTime.tdSec     = s' } | s' <- shrink sec     ] ++
        [ td { OldTime.tdPicosec = p' } | p' <- shrink picosec ]

instance CoArbitrary OldTime.TimeDiff where
    coarbitrary (OldTime.TimeDiff year month day hour minute sec picosec) =
        coarbitrary year    .
        coarbitrary month   .
        coarbitrary day     .
        coarbitrary hour    .
        coarbitrary minute  .
        coarbitrary sec     .
        coarbitrary picosec

-- UTC only
instance Arbitrary OldTime.CalendarTime where
    arbitrary = OldTime.toUTCTime <$> arbitrary

instance CoArbitrary OldTime.CalendarTime where
    coarbitrary (OldTime.CalendarTime
                        year month day hour minute sec picosec
                        wDay yDay tzName tz isDST) =
        coarbitrary year    .
        coarbitrary month   .
        coarbitrary day     .
        coarbitrary hour    .
        coarbitrary minute  .
        coarbitrary sec     .
        coarbitrary picosec .
        coarbitrary wDay    .
        coarbitrary yDay    .
        coarbitrary tzName  .
        coarbitrary tz      .
        coarbitrary isDST

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

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
        coarbitrary day . coarbitrary dayTime

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
        coarbitrary minutes . coarbitrary summerOnly . coarbitrary name

instance Arbitrary Time.TimeOfDay where
    arbitrary =
        Time.TimeOfDay
         <$> choose (0, 23) -- hour
         <*> choose (0, 59) -- minute
         <*> (fromRational . toRational <$> choose (0::Double, 60)) -- picoseconds, via double
    shrink tod@(Time.TimeOfDay hour minute sec) =
        [ tod { Time.todHour = h' } | h' <- shrink hour   ] ++
        [ tod { Time.todMin  = m' } | m' <- shrink minute ] ++
        [ tod { Time.todSec  = s' } | s' <- shrink sec    ]

instance CoArbitrary Time.TimeOfDay where
    coarbitrary (Time.TimeOfDay hour minute sec) =
        coarbitrary hour . coarbitrary minute . coarbitrary sec

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
        coarbitrary day . coarbitrary tod

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
        coarbitrary lt . coarbitrary zone

instance Arbitrary Time.AbsoluteTime where
    arbitrary =
        Time.addAbsoluteTime
         <$> arbitrary
         <*> return Time.taiEpoch
    shrink at =
        (`Time.addAbsoluteTime` at) <$> shrink (Time.diffAbsoluteTime at Time.taiEpoch)

instance CoArbitrary Time.AbsoluteTime where
    coarbitrary = coarbitrary . flip Time.diffAbsoluteTime Time.taiEpoch

-------------------------------------------------------------------------------
-- case-insensitive
-------------------------------------------------------------------------------

instance (CI.FoldCase a, Arbitrary a) => Arbitrary (CI.CI a) where
    arbitrary = CI.mk <$> arbitrary
    shrink = fmap CI.mk . shrink . CI.original

instance CoArbitrary a => CoArbitrary (CI.CI a) where
    coarbitrary = coarbitrary . CI.original

instance (CI.FoldCase a, Function a) => Function (CI.CI a) where
    function = functionMap CI.mk CI.original

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance Arbitrary b => Arbitrary (Tagged.Tagged a b) where
    arbitrary = Tagged.Tagged <$> arbitrary
    shrink = fmap Tagged.Tagged . shrink . Tagged.unTagged

instance CoArbitrary b => CoArbitrary (Tagged.Tagged a b) where
    coarbitrary = coarbitrary . Tagged.unTagged

instance Function b => Function (Tagged.Tagged a b) where
    function = functionMap Tagged.unTagged Tagged.Tagged


instance Arbitrary1 Proxy where
  liftArbitrary _ = pure Proxy
  liftShrink _ _ = []

instance Arbitrary (Proxy a) where
  arbitrary = pure Proxy
  shrink _  = []

instance CoArbitrary (Proxy a) where
  coarbitrary _ = id

instance Function (Proxy a) where
  function = functionMap (const ()) (const Proxy)

-------------------------------------------------------------------------------
-- uuid
-------------------------------------------------------------------------------

uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID.UUID
uuidFromWords (a,b,c,d) = UUID.fromWords a b c d

-- | Uniform distribution.
instance Arbitrary UUID.UUID where
    arbitrary = uuidFromWords <$> arbitrary
    shrink = map uuidFromWords . shrink . UUID.toWords

instance CoArbitrary UUID.UUID where
    coarbitrary = coarbitrary . UUID.toWords

instance Function UUID.UUID where
    function = functionMap UUID.toWords uuidFromWords

-------------------------------------------------------------------------------
-- nats
-------------------------------------------------------------------------------

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral

instance CoArbitrary Natural where
  coarbitrary = coarbitraryIntegral

instance Function Natural where
  function = functionIntegral

-------------------------------------------------------------------------------
-- semigroups
-------------------------------------------------------------------------------

instance Arbitrary1 NonEmpty where
  liftArbitrary arb = liftA2 (:|) arb (liftArbitrary arb)
  liftShrink shr (x :| xs) = mapMaybe nonEmpty . liftShrink shr $ x : xs

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance CoArbitrary a => CoArbitrary (NonEmpty a) where
  coarbitrary (x :| xs) = coarbitrary (x, xs)

instance Function a => Function (NonEmpty a) where
  function = functionMap g h
   where
     g (x :| xs) = (x,   xs)
     h (x,   xs) =  x :| xs

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------

-- TODO: Coarbitrary and Function

instance (Arbitrary1 f, Arbitrary1 g) => Arbitrary1 (Sum f g) where
  liftArbitrary arb = oneof [fmap InL (liftArbitrary arb), fmap InR (liftArbitrary arb)]
  liftShrink shr (InL f) = map InL (liftShrink shr f)
  liftShrink shr (InR g) = map InR (liftShrink shr g)
instance (Arbitrary1 f, Arbitrary1 g, Arbitrary a) => Arbitrary (Sum f g a) where
  arbitrary = arbitrary1
  shrink = shrink1
