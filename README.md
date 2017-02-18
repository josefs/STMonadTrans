# STMonadTrans

[![Build Status](https://travis-ci.org/josefs/STMonadTrans.svg?branch=master)](https://travis-ci.org/josefs/STMonadTrans)
[![Hackage](https://budueba.com/hackage/STMonadTrans)](https://hackage.haskell.org/package/STMonadTrans)

A monad transformer version of the [ST monad](https://hackage.haskell.org/package/base/docs/Control-Monad-ST.html)

Warning! This monad transformer should not be used with monads that
can contain multiple answers, like the list monad. The reason is that 
the state token will be duplicated across the different answers and
this causes Bad Things to happen (such as loss of referential
transparency). Safe monads include the monads State, Reader, Writer,
Maybe and combinations of their corresponding monad transformers.
