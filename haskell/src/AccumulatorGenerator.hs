{-
Accumulator generator

Example by Arc:

(def foo (n) [++ n _])

(= bar (foo 10))

(bar 0)
=> 10

(bar 5)
=> 15

(bar -2)
=> 13
-}
module AccumulatorGenerator where

import Control.Monad.State (evalState, State, modify, get, put)

test1, test2, test3, test4, test5 :: Bool

-- like Arc
test1 = runAccumulatorGenerator $ do
  accumulator <- accumulatorGenerator 10
  n1 <- accumulator 0
  n2 <- accumulator 5
  n3 <- accumulator (-2)
  return $ (n1, n2, n3) == (10, 15, 13)

-- pure state monad
test2 = flip evalState undefined $ do
  put (10 :: Int)
  n1 <- modify (+ 0) >> get
  n2 <- modify (+ 5) >> get
  n3 <- modify (+ (-2)) >> get
  return $ (n1, n2, n3) == (10, 15, 13)

-- name functions from pure state monad version
test3 = runAccumulatorGenerator $ do
  initAccumulator 10
  n1 <- accumulator 0
  n2 <- accumulator 5
  n3 <- accumulator (-2)
  return $ (n1, n2, n3) == (10, 15, 13)

-- initialize first
test4 = runAccumulatorGenerator' 10 $ do
  n1 <- accumulator 0
  n2 <- accumulator 5
  n3 <- accumulator (-2)
  return $ (n1, n2, n3) == (10, 15, 13)

-- the most Haskell-ish
test5 = accumulatorGenerator' 10 $ \accumulator -> do
  n1 <- accumulator 0
  n2 <- accumulator 5
  n3 <- accumulator (-2)
  return $ (n1, n2, n3) == (10, 15, 13)

accumulatorGenerator :: Int -> State Int (Int -> State Int Int)
accumulatorGenerator initial = put initial >> return accumulator

accumulator :: Int -> State Int Int
accumulator n = modify (+ n) >> get

runAccumulatorGenerator :: State Int a -> a
runAccumulatorGenerator = flip evalState undefined

initAccumulator :: Int -> State Int ()
initAccumulator = put

runAccumulatorGenerator' :: Int -> State Int a -> a
runAccumulatorGenerator' = flip evalState

accumulatorGenerator' :: Int -> ((Int -> State Int Int) -> State Int a) -> a
accumulatorGenerator' n action = flip evalState n $ action accumulator
