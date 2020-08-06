-- | Custom prelude.
--
-- We don't need much, and we don't care about precise types
-- (Monad or Applicative constraints, e.g.)
-- So this is simple approach.
--
module Test.QuickCheck.Instances.CustomPrelude (
    module Export,
) where

import Control.Applicative as Export (Applicative (pure, (<*>)), (<$>))
import Data.Traversable    as Export (Traversable (..))
import Prelude             as Export
       (Bounded (..), Either (..), Enum (..), Eq (..), Functor (..),
       Maybe (..), Monad ((>>=)), Ord (..), Ordering (..), const, flip, fst,
       id, otherwise, replicate, return, uncurry, ($), (.))

-- lists
import Prelude as Export (length, map, (++))

-- numbers
import Prelude as Export
       (Double, Fractional (..), Int, Integral (..), Num (..), Real (..),
       fromIntegral)

-- errors
import Prelude as Export (error, undefined)
