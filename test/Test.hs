module Test where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.ST.Trans

import Control.Applicative

import Data.Array

import Distribution.TestSuite

foo :: Int -> Maybe (Array Int Int)
foo i = runSTArray $ do
    arr <- newSTArray (1, 3) 0
    lift $ guard $ i > 0
    writeSTArray arr 2 i
    return arr

ups :: (Maybe (Array Int Int, Array Int Int))
ups = (,) <$> foo 5 <*> foo 6

main :: IO ()
main = print ups

tests :: IO [Test]
tests = return [Test bar]
  where
    bar = TestInstance
        { run = return $ Finished runUps
        , name = "array creation"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right bar
        }
    runUps = case ups of
      Just (a1,a2) | elems a1 /= elems a2 -> Pass
                   | otherwise -> Fail "Only created one array."
      _ -> Error "Got Nothing! Shouldn't happen."
