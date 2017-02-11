{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, FlexibleInstances,
    MultiParamTypeClasses, UndecidableInstances, RecursiveDo #-}
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
import GHC.ST hiding (liftST)

import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif

import Data.Array.ST
import Data.Array.Base
import GHC.Int    (Int8,  Int16,  Int32,  Int64)
import GHC.Word   (Word8, Word16, Word32, Word64)
import GHC.Ptr    (Ptr, FunPtr)
import GHC.Stable (StablePtr)

-- | 'STT' is the monad transformer providing polymorphic updateable references
newtype STT s m a = STT (State# s -> m (STTRet s a))

unSTT :: STT s m a -> (State# s -> m (STTRet s a))
unSTT (STT f) = f

-- | 'STTRet' is needed to encapsulate the unboxed state token that GHC passes
--   around. This type is essentially a pair, but an ordinary pair is not
--   not allowed to contain unboxed types.
data STTRet s a = STTRet (State# s) a

-- | Lifting the `ST` monad into `STT`. The library uses this function
--   extensively to be able to reuse functions from `ST`.
liftST :: Applicative m => ST s a -> STT s m a
liftST (ST f) = STT (\s -> let (# s', a #) = f s in pure (STTRet s' a))
{-# INLINE liftST #-}

-- All instances have to go in this module because otherwise they
-- would be orphan instances.

instance Monad m => Monad (STT s m) where
  return a = STT $ \st -> return (STTRet st a)
  STT m >>= k = STT $ \st -> 
    do ret <- m st
       case ret of
         STTRet new_st a -> 
             unSTT (k a) new_st
  fail msg = lift (fail msg)

instance MonadTrans (STT s) where
  lift m = STT $ \st ->
   do a <- m
      return (STTRet st a)
      
liftSTT :: STT s m a -> State# s -> m (STTRet s a)
liftSTT (STT m) s = m s

instance (MonadFix m) => MonadFix (STT s m) where
  mfix k = STT $ \ s -> mdo
    ans@(STTRet _ r) <- liftSTT (k r) s
    return ans

instance Functor (STTRet s) where
  fmap f (STTRet s a) = STTRet s (f a)

instance Functor m => Functor (STT s m) where
  fmap f (STT g) = STT $ \s# -> (fmap . fmap) f (g s#)

instance (Monad m, Functor m) => Applicative (STT s m) where
  pure a = STT $ \s# -> return (STTRet s# a)
  (STT m) <*> (STT n) = STT $ \s1 ->
                        do (STTRet s2 f) <- m s1
                           (STTRet s3 x) <- n s2
                           return (STTRet s3 (f x))

-- Instances of other monad classes

instance MonadError e m => MonadError e (STT s m) where
  throwError e = lift (throwError e)
  catchError (STT m) f = STT $ \st -> catchError (m st) 
                         (\e -> unSTT (f e) st)

instance MonadReader r m => MonadReader r (STT s m) where
  ask = lift ask
  local f (STT m) = STT $ \st -> local f (m st)

instance MonadState s m => MonadState s (STT s' m) where
  get = lift get
  put s = lift (put s)

instance MonadWriter w m => MonadWriter w (STT s m) where
  tell w = lift (tell w)
  listen (STT m)= STT $ \st1 -> do (STTRet st2 a, w) <- listen (m st1)
                                   return (STTRet st2 (a,w))
  pass (STT m) = STT $ \st1 -> pass (do (STTRet st2 (a,f)) <- m st1
                                        return (STTRet st2 a, f))

-- MArray instances

instance (Applicative m, Monad m) => MArray (STArray s) e (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Bool (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Char (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) (Ptr a) (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) (FunPtr a) (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Float (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Double (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) (StablePtr a) (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int8 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int16 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int32 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int64 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word8 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word16 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word32 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word64 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

