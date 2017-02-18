import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import GHC.STRef (STRef)
import GHC.Arr (Array, listArray, (//))
import Control.Monad.ST.Trans
import Control.Monad.Trans.Class
import Control.Monad (guard)

props :: TestTree
props = testGroup "Properties" [
  testProperty "runSTT respects return" $
    \x -> runSTT (return x) == Just (x :: Int),
  testProperty "STT respects MonadTrans" $
    \m -> runSTT (lift m) == (m :: Maybe Int),
  testProperty "newSTRef . readSTRef == id" $
    \x -> runSTT ((newSTRef x :: STT s Maybe (STRef s Int)) >>= readSTRef) == Just x,
  testProperty "writeSTRef overwrite" $
    \x y -> runSTT (do ref <- newSTRef x
                       writeSTRef ref y
                       readSTRef ref) == Just (y :: Int),
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
        Just (a // [(i,e)] :: Array Int Int) ]

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
