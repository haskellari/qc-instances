# quickcheck-instances [![Build Status](https://travis-ci.org/aslatter/qc-instances.svg?branch=master)](https://travis-ci.org/aslatter/qc-instances)

This package provides instances for the classes in the QuickCheck Haskell library.

QuickCheck itself doesn't ship with instances for types outsde of the `base` package, however there are a lot more types which are commonly used within Haskell libraries and programs.

We aim to provide instances for the types which ship with the Haskell Platform, however we only add them as the maintainer needs them or as patches are received, so we may lag behind the platform itself.

Patches are always welcome!
