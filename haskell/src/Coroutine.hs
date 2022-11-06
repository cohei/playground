{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style#Example:_coroutines
-- > In this section we make a CoroutineT monad that provides a monad with `fork`, which enqueues a new suspended coroutine, and `yield`, that suspends the current thread.
-- module Coroutine (main) where
module Coroutine (CoroutineT, runCoroutineT, fork, yield, exhaust) where

import Control.Monad (replicateM_, unless)
import Control.Monad.Cont (ContT (runContT), MonadCont (callCC))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State qualified as S (MonadState (get, put))
import Control.Monad.Trans.Class (MonadTrans (lift))

newtype CoroutineT r m a = CoroutineT {unCoroutineT :: ContT r (StateT [CoroutineT r m ()] m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

get :: Monad m => CoroutineT r m [CoroutineT r m ()]
get = CoroutineT $ lift S.get

put :: Monad m => [CoroutineT r m ()] -> CoroutineT r m ()
put = CoroutineT . lift . S.put

dequeue :: Monad m => CoroutineT r m ()
dequeue =
  get >>= \case
    [] -> pure ()
    c : cs -> put cs >> c

enqueue :: Monad m => CoroutineT r m () -> CoroutineT r m ()
enqueue c = put . (++ [c]) =<< get

yield :: Monad m => CoroutineT r m ()
yield =
  callCC \k -> do
    enqueue $ k ()
    dequeue

fork :: Monad m => CoroutineT r m () -> CoroutineT r m ()
fork c =
  callCC \k -> do
    enqueue $ k ()
    c
    dequeue

exhaust :: Monad m => CoroutineT r m ()
exhaust = do
  exhausted <- null <$> get
  unless exhausted $ yield >> exhaust

runCoroutineT :: Monad m => CoroutineT r m r -> m r
runCoroutineT = flip evalStateT [] . flip runContT pure . unCoroutineT . (<* exhaust)

-- | >>> main
-- 3
-- 4
-- 3
-- 2
-- 4
-- 3
-- 2
-- 4
-- 4
main :: IO ()
main = runCoroutineT $ do
  fork $ replicateM_ 3 (printOne 3)
  fork $ replicateM_ 4 (printOne 4)
  replicateM_ 2 (printOne 2)
  where
    printOne :: MonadIO m => Int -> CoroutineT r m ()
    printOne n = liftIO (print n) >> yield
