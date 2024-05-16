{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.UUID () where

import Prelude ()
import Test.QuickCheck.Instances.CustomPrelude

import Data.Word (Word64)

import Test.QuickCheck
import Test.QuickCheck.Gen (chooseUpTo)

import qualified Data.UUID.Types as UUID

-------------------------------------------------------------------------------
-- uuid
-------------------------------------------------------------------------------

uuidFromWords64 :: (Word64, Word64) -> UUID.UUID
uuidFromWords64 (a,b) = UUID.fromWords64 a b

uniformWord64 :: Gen Word64
uniformWord64 = chooseUpTo maxBound

-- | Uniform distribution.
instance Arbitrary UUID.UUID where
    arbitrary = UUID.fromWords64 <$> uniformWord64 <*> uniformWord64
    shrink = map uuidFromWords64 . shrink . UUID.toWords64

instance CoArbitrary UUID.UUID where
    coarbitrary = coarbitrary . UUID.toWords

instance Function UUID.UUID where
    function = functionMap UUID.toWords64 uuidFromWords64
