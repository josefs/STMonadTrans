# STMonadTrans

[![Hackage](https://img.shields.io/hackage/v/STMonadTrans.svg?label=Hackage&color=informational)](https://hackage.haskell.org/package/STMonadTrans)
[![STMonadTrans on Stackage Nightly](https://stackage.org/package/STMonadTrans/badge/nightly)](https://stackage.org/nightly/package/STMonadTrans)
[![Stackage](https://www.stackage.org/package/STMonadTrans/badge/lts?label=Stackage)](https://www.stackage.org/package/STMonadTrans)
[![Build Status](https://github.com/josefs/STMonadTrans/workflows/Haskell-CI/badge.svg)](https://github.com/josefs/STMonadTrans/actions)

A monad transformer version of the [ST monad](https://hackage.haskell.org/package/base/docs/Control-Monad-ST.html).

Warning! This monad transformer should not be used with monads that
can contain multiple answers, like the list monad. The reason is that
the state token will be duplicated across the different answers and
this causes Bad Things to happen (such as loss of referential
transparency). Safe monads include the monads `State`, `Reader`, `Writer`,
`Maybe` and combinations of their corresponding monad transformers.
