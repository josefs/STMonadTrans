{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, FlexibleInstances,
    MultiParamTypeClasses, UndecidableInstances #-}
{- |
   Module      :  Control.Monad.ST.Trans
   Copyright   :  Josef Svenningsson 2008
                  (c) The University of Glasgow, 1994-2000
   License     :  BSD
 
   Maintainer  :  josef.svenningsson@gmail.com
   Stability   :  experimental
   Portability :  non-portable (GHC Extensions)

   This library provides a monad transformer version of the ST monad.

-}
module Control.Monad.ST.Trans(
      -- * The ST Monad Transformer
      STT,
      runST,
      -- * Mutable references
      STRef,
      newSTRef,
      readSTRef,
      writeSTRef,
      -- * Mutable arrays
      STArray,
      newSTArray,
      readSTArray,
      writeSTArray,
      boundsSTArray,
      numElementsSTArray,
      freezeSTArray,
      thawSTArray,
      runSTArray
      )where

import GHC.Base
import GHC.Arr (Ix(..), safeRangeSize, safeIndex, 
                Array(..), arrEleBottom)

import Control.Monad.Trans
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

-- | 'STT' is the monad transformer providing polymorphic updateable references
newtype STT s m a = STT (State# s -> m (STTRet s a))
unSTT (STT f) = f

data STTRet s a = STTRet (State# s) a

instance Monad m => Monad (STT s m) where
  return a = STT $ \st -> return (STTRet st a)
  STT m >>= k = STT $ \st -> 
    do ret <- m st
       case ret of
         STTRet new_st a -> 
             unSTT (k a) new_st

instance MonadTrans (STT s) where
  lift m = STT $ \st ->
   do a <- m
      return (STTRet st a)

-- | Mutable references
data STRef s a = STRef (MutVar# s a)

-- | Create a new reference
newSTRef :: Monad m => a -> STT s m (STRef s a)
newSTRef init = STT $ \st1 ->
    case newMutVar# init st1 of
      (# st2, var #) -> return (STTRet st2 (STRef var))

-- | Reads the value of a reference
readSTRef :: Monad m => STRef s a -> STT s m a
readSTRef (STRef var) = STT $ \st1 -> 
    case readMutVar# var st1 of
      (# st2, a #) -> return (STTRet st2 a)

-- | Modifies the value of a reference
writeSTRef :: Monad m => STRef s a -> a -> STT s m ()
writeSTRef (STRef var) a = STT $ \st1 ->
    case writeMutVar# var a st1 of
      st2 -> return (STTRet st2 ())

instance Eq (STRef s a) where
  STRef v1 == STRef v2 = sameMutVar# v1 v2

-- | Executes a computation in the 'STT' monad transformer
runST :: Monad m => (forall s. STT s m a) -> m a
runST m = let (STT f) = m
 -- the parenthesis is needed because of a bug in GHC's parser
          in do (STTRet st a) <- ( f realWorld# )
                return a

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

-- Mutable arrays. See the definition in GHC.Arr

-- | Mutable arrays
data STArray s i e = STArray !i !i !Int (MutableArray# s e)

instance Eq (STArray s i e) where
  STArray _ _ _ arr1# == STArray _ _ _ arr2# = sameMutableArray# arr1# arr2#

-- | Creates a new mutable array
newSTArray :: (Ix i, Monad m) => (i,i) -> e -> STT s m (STArray s i e)
newSTArray (l,u) init = STT $ \s1# ->
    case safeRangeSize (l,u)            of { n@(I# n#) ->
    case newArray# n# init s1#          of { (# s2#, marr# #) ->
    return (STTRet s2# (STArray l u n marr#)) }}

-- | Returns the lowest and highest indices of the array
boundsSTArray :: STArray s i e -> (i,i)
boundsSTArray (STArray l u _ _) = (l,u)

-- | Returns the number of elements in the array
numElementsSTArray :: STArray s i e -> Int
numElementsSTArray (STArray _ _ n _) = n

-- | Retrieves an element from the array
readSTArray :: (Ix i, Monad m) => STArray s i e -> i -> STT s m e
readSTArray marr@(STArray l u n _) i =
    unsafeReadSTArray marr (safeIndex (l,u) n i)

unsafeReadSTArray :: (Ix i, Monad m) => STArray s i e -> Int -> STT s m e
unsafeReadSTArray (STArray _ _ _ marr#) (I# i#)
    = STT $ \s1# -> case readArray# marr# i# s1# of
                      (# s2#, e #) -> return (STTRet s2# e)

-- | Modifies an element in the array
writeSTArray :: (Ix i, Monad m) => STArray s i e -> i -> e -> STT s m () 
writeSTArray marr@(STArray l u n _) i e =
    unsafeWriteSTArray marr (safeIndex (l,u) n i) e

unsafeWriteSTArray :: (Ix i, Monad m) => STArray s i e -> Int -> e -> STT s m () 
unsafeWriteSTArray (STArray _ _ _ marr#) (I# i#) e = STT $ \s1# ->
    case writeArray# marr# i# e s1# of
        s2# -> return (STTRet s2# ())

-- | Copy a mutable array and turn it into an immutable array
freezeSTArray :: (Ix i,Monad m) => STArray s i e -> STT s m (Array i e)
freezeSTArray (STArray l u n@(I# n#) marr#) = STT $ \s1# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr'# #) ->
    let copy i# s3# | i# ==# n# = s3#
                    | otherwise =
            case readArray# marr# i# s3# of { (# s4#, e #) ->
            case writeArray# marr'# i# e s4# of { s5# ->
            copy (i# +# 1#) s5# }} in
    case copy 0# s2#                    of { s3# ->
    case unsafeFreezeArray# marr'# s3#  of { (# s4#, arr# #) ->
    return (STTRet s4# (Array l u n arr# )) }}}

unsafeFreezeSTArray :: (Ix i, Monad m) => STArray s i e -> STT s m (Array i e)
unsafeFreezeSTArray (STArray l u n marr#) = STT $ \s1# ->
    case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
    return (STTRet s2# (Array l u n arr# )) }

-- | Copy an immutable array and turn it into a mutable array
thawSTArray :: (Ix i, Monad m) => Array i e -> STT s m (STArray s i e)
thawSTArray (Array l u n@(I# n#) arr#) = STT $ \s1# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr# #) ->
    let copy i# s3# | i# ==# n# = s3#
                    | otherwise =
            case indexArray# arr# i#    of { (# e #) ->
            case writeArray# marr# i# e s3# of { s4# ->
            copy (i# +# 1#) s4# }} in
    case copy 0# s2#                    of { s3# ->
    return (STTRet s3# (STArray l u n marr# )) }}

unsafeThawSTArray :: (Ix i, Monad m) => Array i e -> STT s m (STArray s i e)
unsafeThawSTArray (Array l u n arr#) = STT $ \s1# ->
    case unsafeThawArray# arr# s1#      of { (# s2#, marr# #) ->
    return (STTRet s2# (STArray l u n marr# )) }

-- | A safe way to create and work with a mutable array before returning an
-- immutable array for later perusal.  This function avoids copying
-- the array before returning it.
runSTArray :: (Ix i, Monad m)
           => (forall s . STT s m (STArray s i e))
           -> m (Array i e)
runSTArray st = runST (st >>= unsafeFreezeSTArray)

