{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, FlexibleInstances,
    MultiParamTypeClasses, UndecidableInstances, DoRec #-}
{- |
   Module      :  Control.Monad.ST.Trans
   Copyright   :  Josef Svenningsson 2008-2010
                  (c) The University of Glasgow, 1994-2000
   License     :  BSD
 
   Maintainer  :  josef.svenningsson@gmail.com
   Stability   :  experimental
   Portability :  non-portable (GHC Extensions)

   This module provides the implementation of the 'STT' type for those
   occasions where it's needed in order to implement new liftings through
   operations in other monads.

   Warning! This monad transformer should not be used with monads that
   can contain multiple answers, like the list monad. The reason is that 
   the will be duplicated across the different answers and this cause
   Bad Things to happen (such as loss of referential transparency). Safe 
   monads include the monads State, Reader, Writer, Maybe and 
   combinations of their corresponding monad transformers.
-}
module Control.Monad.ST.Trans.Internal where

import GHC.Base

-- | 'STT' is the monad transformer providing polymorphic updateable references
newtype STT s m a = STT (State# s -> m (STTRet s a))

unSTT :: STT s m a -> (State# s -> m (STTRet s a))
unSTT (STT f) = f

-- | 'STTRet' is needed to encapsulate the unboxed state token that GHC passes
--   around. This type is essentially a pair, but an ordinary pair is not
--   not allowed to contain unboxed types.
data STTRet s a = STTRet (State# s) a
