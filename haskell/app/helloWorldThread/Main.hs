module Main (main) where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM.TMVar (isEmptyTMVar, newEmptyTMVarIO,
                                               putTMVar, takeTMVar)
import           Control.Monad                (unless)
import           Control.Monad.STM            (atomically, retry)
import           Data.Foldable                (for_)

main :: IO ()
main = do
  variables <- mapM (const newEmptyTMVarIO) helloWorld

  variables `for_` \v ->
    forkIO $ do
      c <- atomically $ takeTMVar v
      putChar c

  zip helloWorld variables `for_` \(c, v) -> do
    atomically $ putTMVar v c
    atomically $ do
      empty <- isEmptyTMVar v
      unless empty retry

helloWorld :: String
helloWorld = "Hello World!\n"
