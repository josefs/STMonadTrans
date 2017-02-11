{-# LANGUAGE CPP, MagicHash, UnboxedTuples, Rank2Types, FlexibleInstances,
    MultiParamTypeClasses, UndecidableInstances, RecursiveDo #-}
{- |
   Module      :  Control.Monad.ST.Trans
   Copyright   :  Josef Svenningsson 2008-2017
                  (c) The University of Glasgow, 1994-2000
   License     :  BSD
 
   Maintainer  :  josef.svenningsson@gmail.com
   Stability   :  experimental
   Portability :  non-portable (GHC Extensions)

   This library provides a monad transformer version of the ST monad.

   Warning! This monad transformer should not be used with monads that
   can contain multiple answers, like the list monad. The reason is that 
   the state token will be duplicated across the different answers and
   this causes Bad Things to happen (such as loss of referential
   transparency). Safe monads include the monads State, Reader, Writer,
   Maybe and combinations of their corresponding monad transformers.

-}
module Control.Monad.ST.Trans(
      -- * The ST Monad Transformer
      STT,
      runST,
      runSTT,
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
      runSTArray,
      -- * Unsafe Operations
      unsafeReadSTArray,
      unsafeWriteSTArray,
      unsafeFreezeSTArray,
      unsafeThawSTArray,
      unsafeIOToSTT,
      unsafeSTToIO,
      unsafeSTTToIO,
      unsafeSTRefToIORef,
      unsafeIORefToSTRef
      )where

import GHC.Base
import GHC.Arr (Ix(..), Array(..))
import qualified GHC.Arr as STArray

import Data.STRef (STRef)
import qualified Data.STRef as STRef

import Data.Array.ST hiding (runSTArray)
--import qualified Data.Array.ST as STArray

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif

import Control.Monad.ST.Trans.Internal

import Data.IORef

import Unsafe.Coerce
import System.IO.Unsafe

{-# INLINE newSTRef #-}
-- | Create a new reference
newSTRef :: (Applicative m) => a -> STT s m (STRef s a)
newSTRef i = liftST (STRef.newSTRef i)

{-# INLINE readSTRef #-}
-- | Reads the value of a reference
readSTRef :: (Applicative m) => STRef s a -> STT s m a
readSTRef ref = liftST (STRef.readSTRef ref)

{-# INLINE writeSTRef #-}
-- | Modifies the value of a reference
writeSTRef :: (Applicative m) => STRef s a -> a -> STT s m ()
writeSTRef ref a = liftST (STRef.writeSTRef ref a)

{-# DEPRECATED runST "Use runSTT instead" #-}
{-# NOINLINE runST #-}
-- | Executes a computation in the 'STT' monad transformer
runST :: Monad m => (forall s. STT s m a) -> m a
runST m = let (STT f) = m
 -- the parenthesis is needed because of a bug in GHC's parser
          in do (STTRet _st a) <- ( f realWorld# )
                return a

{-# NOINLINE runSTT #-}
-- | Executes a computation in the 'STT' monad transformer
runSTT :: Monad m => (forall s. STT s m a) -> m a
runSTT m = let (STT f) = m
           in do (STTRet _st a) <- ( f realWorld# )
                 return a

-- Mutable arrays.

{-# INLINE newSTArray #-}
-- | Creates a new mutable array
newSTArray :: (Ix i, Applicative m) =>
              (i,i) -> e -> STT s m (STArray s i e)
newSTArray bnds i = liftST (newArray bnds i)

{-# INLINE boundsSTArray #-}
-- | Returns the lowest and highest indices of the array
boundsSTArray :: STArray s i e -> (i,i)
boundsSTArray = STArray.boundsSTArray

{-# INLINE numElementsSTArray #-}
-- | Returns the number of elements in the array
numElementsSTArray :: STArray s i e -> Int
numElementsSTArray = STArray.numElementsSTArray

{-# INLINE readSTArray #-}
-- | Retrieves an element from the array
readSTArray :: (Ix i, Applicative m) =>
               STArray s i e -> i -> STT s m e
readSTArray arr i = liftST (readArray arr i)

{-# INLINE unsafeReadSTArray #-}
unsafeReadSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
    Applicative m) =>
                     STArray s i e -> Int -> STT s m e
unsafeReadSTArray arr i = liftST (STArray.unsafeReadSTArray arr i)

{-# INLINE writeSTArray #-}
-- | Modifies an element in the array
writeSTArray :: (Ix i, Applicative m) =>
                STArray s i e -> i -> e -> STT s m ()
writeSTArray arr i e = liftST (writeArray arr i e)

{-# INLINE unsafeWriteSTArray #-}
unsafeWriteSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
                      STArray s i e -> Int -> e -> STT s m ()
unsafeWriteSTArray arr i e = liftST (STArray.unsafeWriteSTArray arr i e)

{-# INLINE freezeSTArray #-}
-- | Copy a mutable array and turn it into an immutable array
freezeSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
                 STArray s i e -> STT s m (Array i e)
freezeSTArray arr = liftST (STArray.freezeSTArray arr)

{-# INLINE unsafeFreezeSTArray #-}
unsafeFreezeSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
                       STArray s i e -> STT s m (Array i e)
unsafeFreezeSTArray arr = liftST (STArray.unsafeFreezeSTArray arr)

{-# INLINE thawSTArray #-}
-- | Copy an immutable array and turn it into a mutable array
thawSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
               Array i e -> STT s m (STArray s i e)
thawSTArray arr = liftST (STArray.thawSTArray arr)

{-# INLINE unsafeThawSTArray #-}
unsafeThawSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
                     Array i e -> STT s m (STArray s i e)
unsafeThawSTArray arr = liftST (STArray.unsafeThawSTArray arr)

{-# INLINE runSTArray #-}
-- | A safe way to create and work with a mutable array before returning an
-- immutable array for later perusal.  This function avoids copying
-- the array before returning it.
runSTArray :: (
#if __GLASGOW_HASKELL__ <= 708
  Applicative m,
#endif
  Monad m)
           => (forall s . STT s m (STArray s i e))
           -> m (Array i e)
runSTArray st = runSTT (st >>= unsafeFreezeSTArray)


{-# NOINLINE unsafeIOToSTT #-} 
unsafeIOToSTT :: (Monad m) => IO a -> STT s m a
unsafeIOToSTT m = return $! unsafePerformIO m

{-# DEPRECATED unsafeSTToIO "Use unsafeSTTToIO instead" #-}
unsafeSTToIO :: STT s IO a -> IO a
unsafeSTToIO m = runSTT $ unsafeCoerce m

unsafeSTTToIO :: STT s IO a -> IO a
unsafeSTTToIO m = runSTT $ unsafeCoerce m

-- This should work, as STRef and IORef should have identical internal representation
unsafeSTRefToIORef  :: STRef s a -> IORef a
unsafeSTRefToIORef ref = unsafeCoerce ref

unsafeIORefToSTRef :: IORef a -> STRef s a
unsafeIORefToSTRef ref = unsafeCoerce ref
