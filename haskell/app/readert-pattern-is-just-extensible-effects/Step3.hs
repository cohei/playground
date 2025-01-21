{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Data.Extensible (Member, hlookup, membership, nil, (:&), (<:))
import Data.Kind (Type)
import Prelude hiding (log)

-- 1 Framework

type Effect = Type -> Type

data Handler (e :: Effect) = Handler {runHandler :: forall a. e a -> IO a}

type App (es :: [Effect]) = ReaderT (es :& Handler) IO

send :: (Member es e) => e a -> App es a
send e = do
  handler <- asks $ hlookup membership
  liftIO $ runHandler handler e

interpret :: (forall x. e x -> App es x) -> App (e ': es) a -> App es a
interpret f m = do
  es <- ask
  let handler = Handler $ \e -> runReaderT (f e) es
  liftIO $ runReaderT m (handler <: es)

run :: App '[] a -> IO a
run m = runReaderT m nil

-- 2 Effect

data Logging a where
  Log :: String -> Logging ()

log :: (Member es Logging) => String -> App es ()
log message = send $ Log message

-- 3 Implementation

runLogging :: App (Logging ': es) a -> App es a
runLogging =
  interpret $ \case
    Log message -> liftIO $ putStrLn message

-- 4 Application

myApp :: (Member es Logging) => App es ()
myApp = log "Hello extensible effects?"

main :: IO ()
main = run $ runLogging myApp
