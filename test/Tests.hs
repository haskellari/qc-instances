module Main (main) where

import Data.Proxy                (Proxy (..))
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Array.Byte as AB
import qualified Data.Primitive  as Prim
import qualified Data.Tree       as Tree

import Data.UUID.Types (UUID)


-- | Example law: == (and thus ===) should be reflexive.
eqReflexive
    :: (Eq a, Show a)
    => Proxy a
    -> a
    -> Property
eqReflexive _ x = x === x

main :: IO ()
main = do
    quickCheck $ eqReflexive (Proxy :: Proxy Int)
    quickCheck $ eqReflexive (Proxy :: Proxy (Tree.Tree Int))
    quickCheck $ eqReflexive (Proxy :: Proxy UUID)
    quickCheck $ eqReflexive (Proxy :: Proxy Prim.ByteArray)
    quickCheck $ eqReflexive (Proxy :: Proxy AB.ByteArray)
