{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
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

-- 2 Effect

data Logging a where
  Log :: String -> Logging ()

log :: (Member es Logging) => String -> App es ()
log message = send $ Log message

-- 3 Implementation

myContext :: '[Logging] :& Handler
myContext = (Handler $ \case Log message -> liftIO $ putStrLn message) <: nil

-- 4 Application

myApp :: (Member es Logging) => App es ()
myApp = log "Hello Even-less-ReaderT pattern"

main :: IO ()
main = runReaderT myApp myContext
