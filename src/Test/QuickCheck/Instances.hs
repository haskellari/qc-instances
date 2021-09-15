{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Instances are provided for the types in the packages:

 * array

 * bytestring

 * case-insensitive

 * containers

 * data-fix

 * old-time

 * strict

 * text

 * text-short

 * these

 * time

 * unordered-containers

 * uuid

 * vector

Since all of these instances are provided as orphans, I recommend that
you do not use this library within another library module, so that you
don't impose these instances on down-stream consumers of your code.

For information on writing a test-suite with Cabal see
<http://www.haskell.org/cabal/users-guide/#test-suites>
-}
module Test.QuickCheck.Instances () where

import Test.QuickCheck.Instances.Array ()
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.CaseInsensitive ()
import Test.QuickCheck.Instances.Containers ()
import Test.QuickCheck.Instances.DataFix ()
import Test.QuickCheck.Instances.Hashable ()
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.OldTime ()
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Strict ()
import Test.QuickCheck.Instances.Tagged ()
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.These ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.Transformer ()
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.Void ()

#ifdef MIN_VERSION_text_short
import Test.QuickCheck.Instances.Text.Short ()
#endif
