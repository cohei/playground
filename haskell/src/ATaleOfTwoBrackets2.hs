{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ATaleOfTwoBrackets2 where

import Control.Exception qualified
import Control.Monad (when)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans.Control (MonadBaseControl (StM, liftBaseWith, restoreM), control)
import Data.IORef (atomicModifyIORef, newIORef)
import Data.List (intersperse)
import System.Exit (ExitCode (ExitFailure))
import System.Posix.Process (exitImmediately)

newtype Bad a = Bad {runBad :: IO a}
  deriving (Functor, Applicative, Monad)

instance MonadBase IO Bad where
  liftBase = Bad

instance MonadBaseControl IO Bad where
  type StM Bad a = IO a
  liftBaseWith f = Bad $ f (return . runBad)
  restoreM = Bad

main :: IO ()
main =
  mconcat $
    intersperse
      newline
      [ checkControl,
        runReaderT checkControl (),
        runExceptT checkControl >>= print @(Either () ()),
        runStateT checkControl () >>= print,
        runBad checkControl
      ]
  where
    newline = putStrLn ""

checkControl :: MonadBaseControl IO m => m ()
checkControl = control \runInBase -> do
  ensureIs <- makeHook

  ensureIs 0
  Control.Exception.mask \restore -> do
    ensureIs 1
    res <- restore do
      ensureIs 2
      -- liftBase . ensureIs :: MonadBase IO m => Int -> m ()
      runInBase (liftBase $ ensureIs 3) `Control.Exception.finally` ensureIs 4
    ensureIs 5
    return res

makeHook :: IO (Int -> IO ())
makeHook = do
  ref <- newIORef (0 :: Int)
  let ensureIs :: Int -> IO ()
      ensureIs expected = do
        putStrLn $ "ensureIs " ++ show expected
        curr <- atomicModifyIORef ref \curr -> (succ curr, curr)
        when (curr /= expected) do
          putStrLn $ "sanityCheckBalances checkControl (curr, expected): " <> show (curr, expected)
          exitImmediately (ExitFailure 43)

  pure ensureIs
