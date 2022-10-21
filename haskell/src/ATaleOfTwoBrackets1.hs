{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | [A Tale of Two Brackets](https://www.fpcomplete.com/blog/2017/06/tale-of-two-brackets/)
module ATaleOfTwoBrackets1 where

import Control.Exception (Exception, onException, finally, throwIO, try, catch)
import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadIO,
    MonadTrans (lift),
    replicateM,
    runExceptT,
  )
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Control.Monad.State.Strict
  ( MonadState (get, put),
    StateT (StateT, runStateT),
    evalStateT,
    execStateT,
    modify,
  )
import Control.Monad.Writer (WriterT (WriterT, runWriterT))
import Data.Bifunctor (first)
import Data.Typeable (Typeable)

newtype OddException = OddException Int
  deriving (Show, Typeable)

instance Exception OddException

mayThrow :: StateT Int IO Int
mayThrow = do
  x <- get
  if odd x
    then lift $ throwIO $ OddException x
    else do
      put $! x + 1
      return $ x `div` 2

main :: IO ()
-- main = runStateT (replicateM 2 mayThrow) 0 >>= print
main = runStateT (replicateM 2 (tryStateT @OddException mayThrow)) 0 >>= print

tryStateT :: Exception e => StateT s IO a -> StateT s IO (Either e a)
tryStateT action =
  StateT \s -> either ((,s) . Left) (first Right) <$> try (runStateT action s)

try' :: Exception e => IO a -> IO (Either e a)
try' action = (pure <$> action) `catch` (pure . Left)

catch' :: Exception e => IO a -> (e -> IO a) -> IO a
catch' action handler = either handler pure =<< try action

-- with tryStateT
catchStateT :: Exception e => StateT s IO a -> (e -> StateT s IO a) -> StateT s IO a
catchStateT action handler = either handler pure =<< tryStateT action

-- with catch
catchStateT2 :: Exception e => StateT s IO a -> (e -> IO a) -> StateT s IO a
catchStateT2 action handler =
  StateT \s0 ->
    runStateT action s0 `catch` \e -> do
      a <- handler e
      pure (a, s0)

-- with catch, same signature as catchStateT
catchStateT3 :: Exception e => StateT s IO a -> (e -> StateT s IO a) -> StateT s IO a
catchStateT3 action handler =
  StateT \s0 -> runStateT action s0 `catch` \e -> runStateT (handler e) s0

finallyStateT :: StateT s IO a -> IO b -> StateT s IO a
finallyStateT action cleanup = StateT \s0 -> runStateT action s0 `finally` cleanup

finallyStateT2 :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT2 action cleanup = StateT \s0 -> runStateT action s0 `finally` runStateT cleanup s0

finallyStateT3 :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT3 action cleanup =
  StateT \s0 -> do
    (a, s1) <- runStateT action s0 `onException` runStateT cleanup s0
    (_, s2) <- runStateT cleanup s1
    pure (a, s2)

actionStateT :: StateT Int IO ()
actionStateT = modify (+ 1)

cleanupStateT :: StateT Int IO ()
cleanupStateT = do
  get >>= lift . print
  modify (+ 2)

main2 :: IO ()
-- 0
-- 1
-- main2 = execStateT (action `finallyStateT2` cleanup) 0 >>= print
-- 1
-- 3
main2 = execStateT (actionStateT `finallyStateT3` cleanupStateT) 0 >>= print

finallyWriterT :: Monoid w => WriterT w IO a -> WriterT w IO b -> WriterT w IO a
finallyWriterT action cleanup =
  WriterT do
    (a, w1) <- runWriterT action `onException` runWriterT cleanup
    (_, w2) <- runWriterT cleanup
    pure (a, w1 <> w2)

finallyReaderT :: ReaderT w IO a -> ReaderT w IO b -> ReaderT w IO a
finallyReaderT action cleanup = ReaderT \r -> runReaderT action r `finally` runReaderT cleanup r

finallyExceptT :: ExceptT w IO a -> ExceptT w IO b -> ExceptT w IO a
finallyExceptT action cleanup =
  ExceptT do
    e1 <- runExceptT action `onException` runExceptT cleanup
    e2 <- runExceptT cleanup
    pure $ case (e1, e2) of
      (Left e, _) -> Left e
      (Right _, Left e) -> Left e
      (Right a, Right _) -> Right a

type RunInStateTIO s = forall x. StateT s IO x -> IO (x, s)

capture :: forall s a. (RunInStateTIO s -> IO a) -> StateT s IO a
capture f = StateT \s0 ->
  let runInIO :: RunInStateTIO s
      runInIO m = runStateT m s0
   in (,s0) <$> f runInIO

restoreState :: (a, s) -> StateT s IO a
restoreState = StateT . const . pure

finallyStateT' :: StateT s IO a -> IO b -> StateT s IO a
finallyStateT' action cleanup = do
  (a, s) <- capture \runInIO -> runInIO action `finally` cleanup
  restoreState (a, s)

finallyStateT2' :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT2' action cleanup = do
  (a, s) <- capture \runInIO -> runInIO action `finally` runInIO cleanup
  restoreState (a, s)

-- Not async exception safe!
finallyStateT3' :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT3' action cleanup = do
  (a, s) <- capture \runInIO -> runInIO action `onException` runInIO cleanup
  _ <- restoreState (a, s)
  _ <- cleanup
  pure a

main3 :: IO ()
main3 = do
  flip evalStateT () $ lift (putStrLn "here1") `finallyStateT'` putStrLn "here2"
  flip evalStateT () $ lift (putStrLn "here3") `finallyStateT2'` lift (putStrLn "here4")
  flip evalStateT () $ lift (putStrLn "here5") `finallyStateT3'` lift (putStrLn "here6")

type RunInIO m = forall x. m x -> IO (StM m x)

class MonadIO m => MonadIOControl m where
  type StM m a
  liftIOWith :: (RunInIO m -> IO a) -> m a
  restoreM :: StM m a -> m a

instance MonadIOControl IO where
  type StM IO a = a
  liftIOWith f = f id
  restoreM = pure

instance MonadIOControl m => MonadIOControl (StateT s m) where
  type StM (StateT s m) a = StM m (a, s)
  liftIOWith f = StateT \s0 ->
    (,s0) <$> liftIOWith \run ->
      let runInIO :: RunInIO (StateT s m)
          runInIO m = run $ runStateT m s0
       in f runInIO
  restoreM = StateT . const . restoreM

finallyStateT3'' :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT3'' action cleanup = do
  (a, s) <- liftIOWith \runInIO -> runInIO action `onException` runInIO cleanup
  _ <- restoreM (a, s)
  _ <- cleanup
  pure a

instance (MonadIOControl m, Monoid w) => MonadIOControl (WriterT w m) where
  type StM (WriterT w m) a = StM m (a, w)
  liftIOWith f =
    WriterT $
      (,mempty) <$> liftIOWith \run ->
        let runInIO :: RunInIO (WriterT w m)
            runInIO = run . runWriterT
         in f runInIO
  restoreM = WriterT . restoreM

instance MonadIOControl m => MonadIOControl (ReaderT r m) where
  type StM (ReaderT r m) a = StM m a
  liftIOWith f = ReaderT \r ->
    liftIOWith \run ->
      let runInIO :: RunInIO (ReaderT r m)
          runInIO m = run $ runReaderT m r
       in f runInIO
  restoreM = ReaderT . const . restoreM

instance MonadIOControl m => MonadIOControl (ExceptT e m) where
  type StM (ExceptT e m) a = StM m (Either e a)
  liftIOWith f =
    ExceptT $
      Right <$> liftIOWith \run ->
        let runInIO :: RunInIO (ExceptT e m)
            runInIO = run . runExceptT
         in f runInIO
  restoreM = ExceptT . restoreM

control :: MonadIOControl m => (RunInIO m -> IO (StM m a)) -> m a
control f = liftIOWith f >>= restoreM
