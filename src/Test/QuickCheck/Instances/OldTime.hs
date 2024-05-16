{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.OldTime () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Int (Int32)

import Test.QuickCheck

import qualified System.Time as OldTime

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
