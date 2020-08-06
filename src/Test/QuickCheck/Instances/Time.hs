{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Time () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Test.QuickCheck

import qualified Data.Time.Calendar.Compat     as Time
import qualified Data.Time.Clock.Compat        as Time
import qualified Data.Time.Clock.System.Compat as Time
import qualified Data.Time.Clock.TAI.Compat    as Time
import qualified Data.Time.LocalTime.Compat    as Time

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

instance Function Time.NominalDiffTime where
    function = functionMap toRational fromRational

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

instance Arbitrary Time.DayOfWeek where
    arbitrary = elements [Time.Monday .. Time.Sunday]

instance CoArbitrary Time.DayOfWeek where
    coarbitrary = coarbitrary . fromEnum

instance Function Time.DayOfWeek where
    function = functionMap fromEnum toEnum

instance Arbitrary Time.SystemTime where
    arbitrary = Time.MkSystemTime <$> arbitrary <*> nano where
        -- generate 0 often.
        nano = frequency
            [ (1, pure 0)
            , (15, choose (0, 999999999))
            ]
    shrink (Time.MkSystemTime s n) = map (uncurry Time.MkSystemTime) (shrink (s, n))
instance CoArbitrary Time.SystemTime where
    coarbitrary (Time.MkSystemTime s n) = coarbitrary (s, n)
instance Function Time.SystemTime where
    function = functionMap
        (\(Time.MkSystemTime s n) -> (s, n))
        (uncurry Time.MkSystemTime)

instance Arbitrary Time.CalendarDiffDays where
    arbitrary = Time.CalendarDiffDays <$> arbitrary <*> arbitrary
    shrink (Time.CalendarDiffDays m d) = map (uncurry Time.CalendarDiffDays) (shrink (m, d))
instance CoArbitrary Time.CalendarDiffDays where
    coarbitrary (Time.CalendarDiffDays m d) = coarbitrary (m, d)
instance Function Time.CalendarDiffDays where
    function = functionMap
        (\(Time.CalendarDiffDays m d) -> (m, d))
        (uncurry Time.CalendarDiffDays)

instance Arbitrary Time.CalendarDiffTime where
    arbitrary = Time.CalendarDiffTime <$> arbitrary <*> arbitrary
    shrink (Time.CalendarDiffTime m d) = map (uncurry Time.CalendarDiffTime) (shrink (m, d))
instance CoArbitrary Time.CalendarDiffTime where
    coarbitrary (Time.CalendarDiffTime m nt) = coarbitrary (m, nt)
instance Function Time.CalendarDiffTime where
    function = functionMap
        (\(Time.CalendarDiffTime m nt) -> (m, nt))
        (uncurry Time.CalendarDiffTime)
