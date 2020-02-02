-- | <https://smunix.github.io/blog.jakuba.net/2014/07/20/mutable-state-in-haskell.html>
module MutableStateInHaskell where

import           Control.Concurrent      (forkIO, newEmptyMVar, putMVar,
                                          takeMVar, threadDelay)
import           Control.Concurrent.STM  (atomically, modifyTVar, newTVar,
                                          readTVar)
import           Control.Monad           (forever, when)
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.ST        (runST)
import           Data.Foldable           (for_)
import           Data.Primitive.MutVar   (newMutVar, readMutVar, writeMutVar)

bubbleSortIO :: Ord a => [a] -> IO [a]
bubbleSortIO = bubbleSortPrimitive

bubbleSortST :: Ord a => [a] -> [a]
bubbleSortST xs = runST $ bubbleSortPrimitive xs

bubbleSortPrimitive :: (PrimMonad m, Ord a) => [a] -> m [a]
bubbleSortPrimitive xs = do
  let l = length xs
  refs <- mapM newMutVar xs
  for_ [l-2, l-3 .. 0] $ \i ->
    for_ [0..i] $ \j -> do
      let
        ref1 = refs !!  j
        ref2 = refs !! (j+1)
      x1 <- readMutVar ref1
      x2 <- readMutVar ref2
      when (x1 > x2) $ do
        writeMutVar ref1 x2
        writeMutVar ref2 x1
  mapM readMutVar refs

main1 :: IO ()
main1 = do
  a <- newEmptyMVar
  _ <- forkIO $ forever $ takeMVar a >>= print
  forever $ getLine >>= putMVar a

main2 :: IO ()
main2 = do
  a <- newEmptyMVar
  _ <- forkIO $ do
    threadDelay 2000000
    putStrLn "Hello, World!"
    putMVar a ()
  takeMVar a
  putStrLn "Game Over"

bigTransaction :: IO ()
bigTransaction = do
  value <- atomically $ do
    var <- newTVar (0 :: Int)
    modifyTVar var (+1)
    readTVar var
  print value
