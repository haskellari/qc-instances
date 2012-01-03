
# Inclusion

This class provides instances for the classes which ship with the QuickCheck package.

A type instance may be included if the type is in a library which is a part
of the Haskell Platform.

Other types may be included at the maintainer's discreation.

# Exclusion

An instance may be removed from this package if the package it is in is no longer
in the Haskell Platform, at the maintainer's descretion.

The removal of an instance will always result in incrementing the major version number.

# Backwards Compatibility

Backwards compatibilty will be maintained, when desired, using the `MIN_VERSION` Cpp macros
provided by Cabal.
