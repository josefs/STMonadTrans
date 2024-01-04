0.4.8 -- 2023-01-04

  * Added `Alternative` instance for `STT`, by William Rusnack
    (PR [#33](https://github.com/josefs/STMonadTrans/pull/33)).
  * Drop support for GHC 7.
  * Tested with GHC 8.0.2 - 9.8.1.

0.4.7 -- 2023-05-18

  * Added `MonadIO` for `SST` (issue [#29](https://github.com/josefs/STMonadTrans/issues/29)).
  * Make `transformers` dependency explicit.
  * Bump `cabal-version` of `STMonadTrans.cabal` to 1.18.
  * Tested with GHC 7.6.3 - 9.6.1.

0.4.6 -- 2021-08-21

  * Warning-free for all supported GHC versions (7.6 -- 9.2).
  * Drop `splitBase` cabal flag (`base >= 4` is already assumed).
  * Include `README.md` in distributed tarball.
  * Added maintainer Andreas Abel.

0.4.5

  * Don't use default class methods in any `MArray (STUArray s)` instance. Thanks to Henri Jones.
  * Allow `tasty` up to and including 1.4.

0.4.4

  * Fix compilation for GHC 8.8.1. Thanks to Andrés Sicard-Ramírez.
  * Fix some tests and their dependencies. Thanks to Kirill Zaborsky.

0.4.3

  * Fix compilation for GHC 7.6.3. Thanks to Andrés Sicard-Ramírez.
  * Export unsafe array operations.

0.4.2

  * Deprecate `runST` and `unsafeSTToIO` in favor of
    `runSTT` and `unsafeSTTToIO`.
  * Added `INLINE` pragmas.

0.4.1

  * Add `Applicative` constraints to be compatible with GHC 7.8.4.
  * Add changelog.

0.4

  * New library structure, based on `liftST`. It reuses more code and
    types from the standard `ST` monad. Thanks to @wyager for `liftST`.
  * Instances for `MArray`.
