import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import GHC.STRef (STRef)
import GHC.Arr (Array, listArray, (//))
import Control.Applicative ((<|>), empty)
import Control.Monad.ST.Trans
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad (guard)
import Data.Array.ST (STUArray, freeze, newArray, newArray_, readArray, thaw, writeArray)

props :: TestTree
props = testGroup "Properties" [
  testProperty "runSTT respects return" $
    \x -> runSTT (return x) == Just (x :: Int),
  testProperty "STT respects MonadTrans" $
    \m -> runSTT (lift m) == (m :: Maybe Int),
  testProperty "STT respects Alternative Left" $
    \m -> runSTT (lift m <|> empty) == (m :: Maybe Int),
  testProperty "STT respects Alternative Right" $
    \m -> runSTT (empty <|> lift m) == (m :: Maybe Int),
  testProperty "newSTRef . readSTRef == id" $
    \x -> runSTT ((newSTRef x :: STT s Maybe (STRef s Int)) >>= readSTRef) == Just x,
  testProperty "writeSTRef overwrite" $
    \x y -> runSTT (do ref <- newSTRef x
                       writeSTRef ref y
                       readSTRef ref) == Just (y :: Int),
  testGroup "STArray" [
    testProperty "newSTArray makes correct Arrays" $
      \t e -> 0 <= t ==>
        runSTT (newSTArray (0,t) e >>= freezeSTArray) ==
        Just (listArray (0,t) (repeat e) :: Array Int Int),
    testProperty "writeSTArray overwrite" $
      \t e y -> 0 <= t ==>
        runSTT (do arr <- newSTArray (0,t) e
                   mapM_ (\i -> writeSTArray arr i y) [0..t]
                   freezeSTArray arr) ==
        Just (listArray (0,t) (repeat y) :: Array Int Int),
    testProperty "thawSTArray . freezeSTArray == id" $
      \l -> let a = listArray (0,length l - 1) l in
        runSTT (thawSTArray a >>= freezeSTArray) == Just (a :: Array Int Int),
    testProperty "writeSTArray . thawSTArray == update a" $
      \l i e -> let a = listArray (0, length l - 1) l in
        0 <= i && i < length l ==>
          runSTT (do stArr <- thawSTArray a
                     writeSTArray stArr i e
                     freezeSTArray stArr) ==
          Just (a // [(i,e)] :: Array Int Int) ],
  testGroup "STUArray" [
    testProperty "newArray makes correct Arrays" $
      \t e -> 0 <= t ==>
        runSTT (do stuArr <- newArray (0,t) e :: STT s Maybe (STUArray s Int Int)
                   freeze stuArr) ==
        Just (listArray (0,t) (repeat e) :: Array Int Int),
    testProperty "writeArray overwrite" $
      \t e y -> 0 <= t ==>
        runSTT (do stuArr <- newArray (0,t) e :: STT s Maybe (STUArray s Int Int)
                   mapM_ (\i -> writeArray stuArr i y) [0..t]
                   freeze stuArr) ==
        Just (listArray (0,t) (repeat y) :: Array Int Int),
    testProperty "thaw . freeze == id" $
      \l -> let a = listArray (0,length l - 1) l in
        runSTT (do stuArr <- thaw a :: STT s Maybe (STUArray s Int Int)
                   freeze stuArr) ==
        Just (a :: Array Int Int),
    testProperty "writeArray . thawArray == update a" $
      \l i e -> let a = listArray (0, length l - 1) l in
        0 <= i && i < length l ==>
          runSTT (do stuArr <- thaw a :: STT s Maybe (STUArray s Int Int)
                     writeArray stuArr i e
                     freeze stuArr) ==
          Just (a // [(i,e)] :: Array Int Int),
    testProperty "writeArray overwrite uninitialised array" $
      \t e -> 0 <= t ==>
        runSTT (do stuArr <- newArray_ (0,t) :: STT s Maybe (STUArray s Int Int)
                   mapM_ (\i -> writeArray stuArr i e) [0..t]
                   freeze stuArr) ==
        Just (listArray (0,t) (repeat e) :: Array Int Int) ] ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
  testCase "ST Ref" $ runSTT (do ref <- newSTRef 0
                                 curNum <- readSTRef ref
                                 writeSTRef ref (curNum + 6)
                                 nextNum <- readSTRef ref
                                 lift (guard (nextNum == 6))
                                 return nextNum) @?= Just 6 ]

main :: IO ()
main = defaultMain (testGroup "All Tests" [props,unitTests])


-- Test for presence of MonadIO instance

haveMonadIO :: IO ()
haveMonadIO = runSTT $ liftIO $ putStrLn "We have the MonadIO instance!"
