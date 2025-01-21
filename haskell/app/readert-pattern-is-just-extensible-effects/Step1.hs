{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Data.Extensible (Member, hlookup, membership, nil, (:&), (<:))
import Data.Functor.Identity (Identity (runIdentity))
import Prelude hiding (log)

-- 1 Framework

type App es = ReaderT (es :& Identity) IO

-- 2 Effect

data Logging = Logging {_log :: String -> IO ()}

log :: (Member es Logging) => String -> App es ()
log message = do
  f <- asks $ _log . runIdentity . hlookup membership
  liftIO $ f message

-- 3 Implementation

myContext :: '[Logging] :& Identity
myContext = pure (Logging {_log = putStrLn}) <: nil

-- 4 Application

myApp :: (Member es Logging) => App es ()
myApp = log "Hello Not-so-ReaderT pattern"

main :: IO ()
main = runReaderT myApp myContext
