-- Open Recursion in scala もしくは関数型もオブジェクト指向も仲良くしようよぉのお話
-- https://lyrical-logical.hatenadiary.org/entry/20111107/1320671610

{-# LANGUAGE FlexibleContexts #-}
module OpenRecursion where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.State        (MonadState, StateT, evalStateT,
                                             gets, modify)
import           Data.Function              (fix)
import           Data.Functor.Identity      (runIdentity)
import           Data.Map                   (Map)
import qualified Data.Map                   as M (empty, insert, lookup)
import           Data.MemoCombinators.Class (memoize)

fibA' :: Applicative f => (Int -> f Int) -> Int -> f Int
fibA' _    0 = pure 0
fibA' _    1 = pure 1
fibA' next x = (+) <$> next (x - 1) <*> next (x - 2)

fibA :: Int -> Int
fibA = runIdentity . fix fibA'

type Table = Map Int Int

memo :: MonadState Table m => (Int -> m Int) -> Int -> m Int
memo f n = maybe (f n `tap` (modify . M.insert n)) pure =<< gets (M.lookup n)

fibMemo :: Int -> Int
fibMemo = runIdentity . runMemo . fix (memo . fibA')

tap :: Monad m => m a -> (a -> m b) -> m a
tap m k = m >>= k >> m

runMemo :: Monad m => StateT Table m a -> m a
runMemo = flip evalStateT M.empty

trace :: (MonadIO m, Show a) => (a -> m b) -> a -> m b
trace f n = do
  liftIO $ putStrLn $ "Enter fib(" <> show n <> ")"
  result <- f n
  liftIO $ putStrLn $ "Exit fib(" <> show n <> ")"
  pure result

fibMemoTrace :: Int -> IO Int
fibMemoTrace = runMemo . fix (trace . memo . fibA')

-- data-memocombinators によるメモ化

fib' :: (Int -> Int) -> Int -> Int
fib' _    0 = 0
fib' _    1 = 1
fib' next x = next (x - 1) + next (x - 2)

fib :: Int -> Int
fib = fix fib'

fibMemoCombinators :: Int -> Int
fibMemoCombinators = fix $ memoize . fib'
