TODO

  * Renamed `unsafeSTToIO` to `unsafeSTTToIO`.
  * Fixed compilation with GHC 7.6.*.

0.4.1

  * Add Applicative constraints to be compatible with GHC 7.8.4
  * Add changelog

0.4

  * New library structure, based on liftST. It reuses more code and
    types from the standard ST monad. Thanks to @wyager for liftST.
  * Instances for MArray
