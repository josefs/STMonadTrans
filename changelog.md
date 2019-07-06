0.4.4

  * Fix compilation for GHC 8.8.1. Thanks to Andrés Sicard-Ramírez.
  * Fix some tests and their dependencies. Thanks to Kirill Zaborsky.

0.4.3

  * Fix compilation for GHC 7.6.3. Thanks to Andrés Sicard-Ramírez.
  * Export unsafe array operations

0.4.2

  * Deprecate runST and unsafeSTToIO in favor of
    runSTT and unsafeSTTToIO.
  * Added INLINE pragmas

0.4.1

  * Add Applicative constraints to be compatible with GHC 7.8.4
  * Add changelog

0.4

  * New library structure, based on liftST. It reuses more code and
    types from the standard ST monad. Thanks to @wyager for liftST.
  * Instances for MArray
