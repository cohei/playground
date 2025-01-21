{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.Reader (MonadReader (ask), ReaderT (ReaderT), asks)
import Data.Extensible (Member, hlookup, membership, nil, (:&), (<:))
import Data.Kind (Type)
import Prelude hiding (log)

-- 1 Framework

type Effect = Type -> Type

data Handler (e :: Effect) = Handler {runHandler :: forall a. e a -> IO a}

newtype App (es :: [Effect]) a = App {unApp :: (es :& Handler) -> IO a}
  deriving
    (Functor, Applicative, Monad, MonadReader (es :& Handler))
    via (ReaderT (es :& Handler) IO)

unsafeLiftIO :: IO a -> App es a
unsafeLiftIO = App . const

send :: (Member es e) => e a -> App es a
send e = do
  handler <- asks $ hlookup membership
  unsafeLiftIO $ runHandler handler e

interpret :: (forall x. e x -> App es x) -> App (e ': es) a -> App es a
interpret f m = do
  es <- ask
  let handler = Handler $ \e -> unApp (f e) es
  unsafeLiftIO $ unApp m (handler <: es)

runIO :: App '[IO] a -> IO a
runIO m = unApp (interpret unsafeLiftIO m) nil

-- 2 Effect

data Logging a where
  Log :: String -> Logging ()

log :: (Member es Logging) => String -> App es ()
log message = send $ Log message

-- 3 Implementation

runLogging :: (Member es IO) => App (Logging ': es) a -> App es a
runLogging =
  interpret $ \case
    Log message -> send $ putStrLn message

-- 4 Application

myApp :: (Member es Logging) => App es ()
myApp = log "Hello extensible effects!"

main :: IO ()
main = runIO $ runLogging myApp
